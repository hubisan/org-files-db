# org-files-db - Agent Instructions

Rust CLI that indexes Org files into SQLite and answers queries over them.

## Start here

- Work is tracked as GitHub issues in `hubisan/org-files-db`; the active issue defines
  scope and acceptance criteria. Prefer the most downstream artifact:
  `ticket > spec > conversation`. See `docs/agents/issue-tracker.md`.
- Stable context, goals, non-goals: `.project/tasks/project-context.org`. Public facts:
  `docs/README.org`. Later phases and backlog are GitHub issues.
- Implementation, review, model routing, correction budget or completion workflow: read
  `docs/agents/WORKFLOW.md`.
- Read history (`.project/tasks/archive/`, `.project/notes/`, `CHANGELOG.org`) only when the
  active issue requires it. Ignore `.project/manual-tests/`, `.project/local/` and
  `old-files/` unless asked.

## Rules

- Chat in the user's language. Write all repository content in English.
- Org files (docs, changelog): bold `*bold*`, code `~name~`, lists `-`, no manual line
  breaks. Prefix source-block lines starting with `*` or `#+` with a comma.
- Small, focused changes; no unrelated refactors. Do not change dependencies unless asked.
- Never touch secrets, `.env`, production configs or credentials.
- Branches: `<type>/<slug>` with `feat`, `fix`, `refactor`, `perf`, `docs`, `test`, `ci`,
  `chore`. Commits: Conventional Commits, English. Add `Refs: #<issue>`. Do not merge unless
  asked.
- Ask only for unclear scope, risky or irreversible choices; otherwise state a small
  assumption and continue.

## Checks and docs

- Run `make ci` (fmt, clippy, test, build) before declaring work done. State exactly
  which checks were not run.
- Update docs in the same change: CLI -> `docs/cli.org`, config -> `docs/config.org`,
  parser/indexing -> the relevant file in `docs/`, README when claims go stale,
  `CHANGELOG.org` for user-visible changes.

## Parser validation

Prefer Orgize as the implementation parser. Use Emacs Org-mode
(`org-element-parse-buffer`) as a reference oracle for tricky syntax (planning lines,
timestamps, special properties, tags, drawers, agenda semantics). Document any
Orgize/Emacs disagreement in the issue before choosing behavior. Never evaluate unsafe
Emacs Lisp, diary expressions or `.dir-locals.el` forms from project files.

## Agent skills

### Issue tracker

Issues and specs are GitHub issues in `hubisan/org-files-db`. See `docs/agents/issue-tracker.md`.

### Triage labels

Use the default mattpocock/skills vocabulary. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context repo; domain docs are created lazily. See `docs/agents/domain.md`.

This block belongs in `AGENTS.md`; `CLAUDE.md` only imports it.
