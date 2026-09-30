use std::{io::Write, time::Instant};

use crate::{
    config::Config,
    db::open_existing_database_read_only,
    presentation::{PresentationSpec, PresentationSpecError},
    presentation_view::{
        register_presentation_view, remove_presentation_view, show_presentation_view,
        wait_for_presentation_view_until, PresentationViewDefinition, PresentationViewInclude,
        ViewControlClientError, VIEW_READ_TIMEOUT,
    },
    presentation_view_cache::{PresentationViewCacheReadError, PresentationViewCacheStore},
    query::{
        parse_query, query_depends_on_relative_dates, sqlite_query_validation_options,
        validate_query, QueryInclude,
    },
};

use super::{
    args::{CliQueryInclude, CliQueryOutput, ViewCommand},
    error::CliError,
    write_json_output,
};

pub(super) fn run_view_command(
    command: ViewCommand,
    writer: &mut impl Write,
) -> Result<(), CliError> {
    match command {
        ViewCommand::Register {
            config,
            output,
            include,
            presentation_spec_json,
            name,
            query,
        } => {
            let config = Config::load_from_file(config).map_err(CliError::Config)?;
            let definition = presentation_view_definition(
                &config,
                name,
                query,
                output,
                &include,
                &presentation_spec_json,
            )?;
            let registration =
                register_presentation_view(&config, definition).map_err(CliError::ViewControl)?;
            write_json_output(writer, &registration)
        }
        ViewCommand::Show { config, name } => {
            let config = Config::load_from_file(config).map_err(CliError::Config)?;
            let view = show_presentation_view(&config, name).map_err(CliError::ViewControl)?;
            write_json_output(writer, &view)
        }
        ViewCommand::Read { config, name } => {
            let config = Config::load_from_file(config).map_err(CliError::Config)?;
            read_presentation_view_payload(&config, &name, writer)
        }
        ViewCommand::Remove { config, name } => {
            let config = Config::load_from_file(config).map_err(CliError::Config)?;
            let removal = remove_presentation_view(&config, name).map_err(CliError::ViewControl)?;
            write_json_output(writer, &removal)
        }
    }
}

pub(super) fn read_presentation_view_payload(
    config: &Config,
    name: &str,
    writer: &mut impl Write,
) -> Result<(), CliError> {
    let deadline = Instant::now() + VIEW_READ_TIMEOUT;
    loop {
        let ticket = wait_for_presentation_view_until(config, name.to_string(), deadline)
            .map_err(CliError::ViewControl)?;
        let store = PresentationViewCacheStore::for_session(config, ticket.view.session_id.clone())
            .map_err(CliError::PresentationViewCachePath)?;
        match store.open_valid(
            &ticket.view,
            &ticket.database_id,
            ticket.generation,
            ticket.effective_query_date.as_deref(),
        ) {
            Ok(mut reader) => {
                reader
                    .copy_payload_to(writer)
                    .map_err(CliError::PresentationViewCacheRead)?;
                return Ok(());
            }
            Err(source) if cache_read_target_changed(&source) => {
                if Instant::now() >= deadline {
                    return Err(CliError::ViewControl(ViewControlClientError::ReadTimeout {
                        name: name.to_string(),
                    }));
                }
            }
            Err(source) => return Err(CliError::PresentationViewCacheRead(source)),
        }
    }
}

pub(super) fn cache_read_target_changed(source: &PresentationViewCacheReadError) -> bool {
    matches!(
        source,
        PresentationViewCacheReadError::InvalidDatabase { .. }
            | PresentationViewCacheReadError::InvalidGeneration { .. }
            | PresentationViewCacheReadError::InvalidEffectiveQueryDate { .. }
            | PresentationViewCacheReadError::InvalidViewRevision { .. }
            | PresentationViewCacheReadError::InvalidViewDefinition { .. }
    )
}

pub(super) fn presentation_view_definition(
    config: &Config,
    name: String,
    query: String,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    presentation_spec_json: &str,
) -> Result<PresentationViewDefinition, CliError> {
    if name.trim().is_empty() {
        return Err(CliError::InvalidPresentationViewUsage(
            "presentation view name must not be empty".to_string(),
        ));
    }

    let connection =
        open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
    let parsed = parse_query(&query).map_err(CliError::QueryParse)?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(CliError::QueryExecute)?;
    let validated = validate_query(parsed, &validation_options).map_err(CliError::QueryValidate)?;
    let spec =
        PresentationSpec::parse_json(presentation_spec_json).map_err(CliError::PresentationSpec)?;
    spec.validate_for_query_target(validated.target)
        .map_err(CliError::PresentationSpec)?;

    let explicit_includes = includes
        .iter()
        .copied()
        .map(QueryInclude::from)
        .collect::<Vec<_>>();
    spec.combined_includes_for_query_target(validated.target, &explicit_includes)
        .map_err(CliError::PresentationSpec)?;

    let presentation_spec = serde_json::from_str(presentation_spec_json)
        .map_err(|source| CliError::PresentationSpec(PresentationSpecError::Json(source)))?;
    Ok(PresentationViewDefinition {
        name,
        query,
        output: output.into(),
        includes: includes
            .iter()
            .copied()
            .map(PresentationViewInclude::from)
            .collect(),
        query_timezone: config.query.timezone.clone(),
        relative_date_dependent: query_depends_on_relative_dates(&validated),
        presentation_spec,
    })
}
