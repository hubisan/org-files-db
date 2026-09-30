use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

use crate::{
    presentation_view::{PresentationViewInclude, PresentationViewOutputMode},
    query::{QueryInclude, QueryOutputMode},
};

#[derive(Debug, Parser)]
#[command(name = "orgfdb", version, about = "Minimal Org files database CLI")]
pub(super) struct Cli {
    #[command(subcommand)]
    pub(super) command: Command,
}

#[derive(Debug, Subcommand)]
pub(super) enum Command {
    Rebuild {
        #[arg(long)]
        config: PathBuf,
        #[arg(long)]
        allow_empty: bool,
        #[arg(
            long,
            help = "Accept changed configured directory-root identities for this manual rebuild"
        )]
        accept_source_root_changes: bool,
    },
    #[command(
        about = "Watch configured Org inputs and apply incremental updates",
        long_about = "Watch configured Org inputs and apply incremental reconciliations. Supported on Unix-like systems only. Routine activity is silent; lifecycle messages and errors are written to stderr."
    )]
    Watch {
        #[arg(long)]
        config: PathBuf,
    },
    Headings {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        no_root: bool,
        #[arg(
            long,
            help = "Deprecated compatibility flag; root rows are included by default"
        )]
        include_root: bool,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    Links {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    #[command(
        about = "Read the committed database identity, index generation, and canonical path"
    )]
    Status {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    #[command(about = "Read committed affected-file changes after a generation")]
    Changes {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        database_id: String,
        #[arg(long)]
        since_generation: i64,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    #[command(about = "Manage session-local presentation views in the active watcher")]
    View {
        #[command(subcommand)]
        command: ViewCommand,
    },
    #[command(name = "__presentation-view-rebuild-worker", hide = true)]
    PresentationViewRebuildWorker {
        #[arg(long)]
        db: PathBuf,
        #[arg(long)]
        cache_root: PathBuf,
        #[arg(long)]
        database_id: String,
        #[arg(long)]
        generation: i64,
        #[arg(long)]
        effective_query_date: Option<String>,
    },
    Query {
        #[command(flatten)]
        format: CliQueryFormatArgs,
        #[arg(long, value_enum, default_value_t = CliQueryOutput::Flat)]
        output: CliQueryOutput,
        #[arg(long, value_enum, value_delimiter = ',')]
        include: Vec<CliQueryInclude>,
        #[arg(
            long,
            value_name = "PATH_OR_DASH",
            help = "Read a JSON array of canonical file paths from PATH, or from stdin with '-'"
        )]
        restrict_files_json: Option<String>,
        #[arg(
            long,
            value_name = "JSON",
            help = "PresentationSpec JSON; valid only with --format presentation-json"
        )]
        presentation_spec_json: Option<String>,
        #[arg(long)]
        config: Option<PathBuf>,
        #[arg(help = "Structural query expression, for example '(todo \"NEXT\")'")]
        query: String,
    },
    Search {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long, conflicts_with = "body")]
        title: bool,
        #[arg(long, conflicts_with = "title")]
        body: bool,
        #[arg(long)]
        config: Option<PathBuf>,
        #[arg(help = "Raw SQLite FTS5 MATCH expression")]
        expression: String,
    },
}

#[derive(Debug, Subcommand)]
pub(super) enum ViewCommand {
    #[command(about = "Register or replace a session-local presentation view")]
    Register {
        #[arg(long)]
        config: PathBuf,
        #[arg(long, value_enum, default_value_t = CliQueryOutput::Flat)]
        output: CliQueryOutput,
        #[arg(long, value_enum, value_delimiter = ',')]
        include: Vec<CliQueryInclude>,
        #[arg(
            long,
            value_name = "JSON",
            help = "PresentationSpec JSON for this view"
        )]
        presentation_spec_json: String,
        #[arg(help = "Session-local view name")]
        name: String,
        #[arg(help = "Structural query expression, for example '(todo \"NEXT\")'")]
        query: String,
    },
    #[command(about = "Show the active registration for a presentation view")]
    Show {
        #[arg(long)]
        config: PathBuf,
        #[arg(help = "Session-local view name")]
        name: String,
    },
    #[command(about = "Read the current materialized presentation view")]
    Read {
        #[arg(long)]
        config: PathBuf,
        #[arg(help = "Session-local view name")]
        name: String,
    },
    #[command(about = "Remove a presentation view from the active watcher session")]
    Remove {
        #[arg(long)]
        config: PathBuf,
        #[arg(help = "Session-local view name")]
        name: String,
    },
}

#[derive(Debug, Clone, Copy, Args)]
pub(super) struct CliOutputArgs {
    #[arg(long, value_enum, default_value_t = CliOutputFormat::Json, conflicts_with = "json")]
    pub(super) format: CliOutputFormat,
    #[arg(
        long,
        conflicts_with = "format",
        help = "Compatibility form for --format json"
    )]
    pub(super) json: bool,
}

impl CliOutputArgs {
    pub(super) fn selected(self) -> CliOutputFormat {
        if self.json {
            CliOutputFormat::Json
        } else {
            self.format
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(super) enum CliOutputFormat {
    Json,
}

#[derive(Debug, Clone, Copy, Args)]
pub(super) struct CliQueryFormatArgs {
    #[arg(
        long,
        value_enum,
        default_value_t = CliQueryOutputFormat::Json,
        conflicts_with = "json"
    )]
    pub(super) format: CliQueryOutputFormat,
    #[arg(
        long,
        conflicts_with = "format",
        help = "Compatibility form for --format json"
    )]
    pub(super) json: bool,
}

impl CliQueryFormatArgs {
    pub(super) fn selected(self) -> CliQueryOutputFormat {
        if self.json {
            CliQueryOutputFormat::Json
        } else {
            self.format
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(super) enum CliQueryOutputFormat {
    Json,
    PresentationJson,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(super) enum CliQueryOutput {
    Flat,
    Outline,
}

impl From<CliQueryOutput> for QueryOutputMode {
    fn from(value: CliQueryOutput) -> Self {
        match value {
            CliQueryOutput::Flat => Self::Flat,
            CliQueryOutput::Outline => Self::Outline,
        }
    }
}

impl From<CliQueryOutput> for PresentationViewOutputMode {
    fn from(value: CliQueryOutput) -> Self {
        match value {
            CliQueryOutput::Flat => Self::Flat,
            CliQueryOutput::Outline => Self::Outline,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(super) enum CliQueryInclude {
    Path,
    Properties,
    EffectiveProperties,
    Keywords,
    Links,
    Backlinks,
    Source,
    Target,
}

impl From<CliQueryInclude> for QueryInclude {
    fn from(value: CliQueryInclude) -> Self {
        match value {
            CliQueryInclude::Path => Self::Path,
            CliQueryInclude::Properties => Self::Properties,
            CliQueryInclude::EffectiveProperties => Self::EffectiveProperties,
            CliQueryInclude::Keywords => Self::Keywords,
            CliQueryInclude::Links => Self::Links,
            CliQueryInclude::Backlinks => Self::Backlinks,
            CliQueryInclude::Source => Self::Source,
            CliQueryInclude::Target => Self::Target,
        }
    }
}

impl From<CliQueryInclude> for PresentationViewInclude {
    fn from(value: CliQueryInclude) -> Self {
        match value {
            CliQueryInclude::Path => Self::Path,
            CliQueryInclude::Properties => Self::Properties,
            CliQueryInclude::EffectiveProperties => Self::EffectiveProperties,
            CliQueryInclude::Keywords => Self::Keywords,
            CliQueryInclude::Links => Self::Links,
            CliQueryInclude::Backlinks => Self::Backlinks,
            CliQueryInclude::Source => Self::Source,
            CliQueryInclude::Target => Self::Target,
        }
    }
}
