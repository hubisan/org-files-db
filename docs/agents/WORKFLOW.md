# Agent workflow

## Work artifact order

Prefer the most downstream existing artifact: `ticket > spec > conversation`. Do not
repeat discovery that the active issue already settles.

Normal flow: `/to-spec` -> `/to-tickets` (usually done by the user) -> planner plans one
ticket -> `implementer` subagent -> focused tests -> planner review -> at most one
correction -> `make ci` -> commit -> PR.

## Roles and model routing

| Role | Claude Code |
| --- | --- |
| Planner and reviewer | main session, Opus 5.5, effort low |
| Implementer (default) | subagent `implementer`: Sonnet, effort medium |
| Escalated implementer | subagent `implementer-escalated`: Sonnet, effort high |

- The planner is the main session; the user selects its model and effort (`/model`,
  `/effort`). Subagent model and effort are fixed in `.claude/agents/*.md`.
- The planner reads the issue, writes a short plan (owned files, seam, focused test
  command, relevant docs section), and delegates to `implementer`.
- Use one implementer by default. Start several only for genuinely independent work with
  disjoint files.
- The implementer fixes ordinary test failures itself and returns a compact report.
- The planner reviews. For tickets that change transaction boundaries, change planning,
  watcher recovery or the DB schema, the planner may raise its own effort to medium for
  the review.
- `implementer-escalated` and any `xhigh`/`max` effort require explicit user approval.
- Git/GitHub housekeeping (branch, commit, push, PR, issues, labels) stays in the planner
  session; delegating it costs more context than it saves.
- Other harnesses keep the same three roles with their own models.

## Skills

Invoke a matching skill through the Skill tool (other harnesses: open its `SKILL.md`).

| Situation | Skill |
| --- | --- |
| Review of a ticket that changes code (`src/`, `tests/`, `sql/`) | `code-review` (planner, before commit) |
| Implementing a ticket with behavior change | `tdd` (implementer) |
| Hard bug, flaky or unexplained failure | `diagnosing-bugs` |
| A term or durable decision is being settled (glossary, ADR) | `domain-modeling` |
| Turning a settled conversation into a spec / tickets | `to-spec`, then `to-tickets` |
| Editing `AGENTS.md`, `CLAUDE.md`, agent docs or skills | `writing-for-agents` |

Skip `code-review` for docs-only, config-only or purely mechanical changes; the planner's
own diff review is enough there.

`code-review` fixed point: the commit before the ticket's first commit (or `HEAD` plus
working tree before committing). Spec source: the active GitHub issue. Standards source:
`AGENTS.md`.

## Correction budget

Initial implementation, including its own red/green fixes, does not count. Maximum
correction cycles after the first review: **1**.

A finding is **substantial** when an acceptance criterion is missing or wrong, a
repository rule is violated, behavior is incorrect, or a test is missing for changed
behavior. Naming and style judgement calls are not substantial: report them, fix them
only inside an already-planned correction.

1. First review finds a substantial issue: the planner adjusts the plan and delegates one
   focused correction.
2. Second review still finds a substantial issue: stop and ask the user. No further
   correction and no escalation without approval.

Also stop and ask when a failure exposes a wrong architectural assumption, a material
scope change, a needed schema or dependency change, or the same structural failure
repeating. Final `make ci` fmt/clippy fixes are not a correction cycle; a failing test
that needs a code change is.

## Testing and context budget

- The implementer runs the narrowest relevant tests while working; the planner runs
  `make ci` once at the end of the ticket.
- Keep command output compact; inspect detailed logs only when a step fails.
- Load context progressively. Subagent briefs name the exact files and sections so the
  implementer does not rediscover them. Do not scan `.claude/skills/`.

## Tests

- Edge cases live in the tests of the unit that owns them (parser, tag, query, config and
  so on), not in indexer or CLI tests.
- Indexer and CLI tests keep one or two end-to-end smoke tests per feature plus contract
  and exit-code checks. Do not repeat a lower-layer test at a higher layer.
- Prefer table-driven tests over several near-identical test functions.
- Parser output is checked by snapshots: each `tests/data/parser/**/fixture.org` has a
  `snapshot.json` (serialized `ParsedOrgDocument`, byte offsets included) compared by
  `tests/parser_snapshots.rs`. Add a fixture instead of field-by-field assertions. After an
  intended parser change run `UPDATE_SNAPSHOTS=1 cargo test --test parser_snapshots`, review
  the snapshot diff, bump `PARSER_INDEXER_CONTRACT_VERSION` and update
  `tests/data/parser/CONTRACT` (the guard hashes the snapshots). Keep targeted tests only
  for specific Emacs behavior a snapshot does not document.
- Unit tests use the shared helpers in `crate::test_support` (`TestDir`, `write_file`).
  Extend that module instead of defining a local copy. Integration tests in `tests/`
  cannot reach it and keep their own helpers.

## Commits and completion

- One commit per completed ticket after review and `make ci`, on a `<type>/<slug>` branch
  (see `AGENTS.md`), with `Refs: #<issue>`. The PR body says `Closes #<issue>`.
- Before declaring a ticket complete: review the diff against the issue, run `make ci`,
  update docs and `CHANGELOG.org` as `AGENTS.md` requires, and state what was
  implemented, what was tested and what remains untested.
