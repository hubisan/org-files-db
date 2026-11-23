# Agent workflow

## Work artifact order

Prefer the most downstream existing artifact: `ticket > spec > conversation`. Do not
repeat discovery that the active issue already settles.

Normal flow: `/to-spec` -> `/to-tickets` (usually done by the user) -> planning subagent -> `implementer` subagent -> focused tests -> independent review subagent -> at most one
correction -> `make ci` -> commit -> PR.

## Roles and model routing

**Codex Luna max fallback:** The preferred effort for `planner-light`, `implementer`
and `reviewer-light` is `max`. If the current Codex client rejects `max`
for a subagent, retry that same role using `gpt-6-luna` with
`model_reasoning_effort = "xhigh"`. This specific fallback is preapproved;
do not block the task waiting for another approval. Because subagent TOML
sets a fixed effort, first adjust the affected agent configuration for the
current run or use a supported explicit per-spawn override; a prose instruction
alone cannot change the effective effort. Keep persistent configuration changes
in a reviewed change, rather than silently committing them. Record the
requested effort, effective fallback and available runtime evidence. If `xhigh`
also fails, stop and ask instead of silently changing model families.


The main session coordinates the workflow, Git and PRs. Choose a cost-effective
session model. Delegate planning, implementation and behavior-changing reviews
to fixed-model subagents. Route by risk before size: security, data writes,
migrations, concurrency and architecture changes require the complex role.

| Task | Subagent | Codex | Claude Code |
| --- | --- | --- | --- |
| Simple planning | `planner-light` | Luna max | Sonnet medium |
| Complex or risky planning | `planner` | Sol medium | Opus medium |
| Small implementation | `implementer-light` | Luna high | Sonnet low |
| Normal implementation | `implementer` | Luna max | Sonnet medium |
| Complex implementation | `implementer-complex` | Sol medium | Opus medium |
| Optional focused test triage | `tester` | Luna high | Sonnet low |
| Normal code review | `reviewer-light` | Luna max | Sonnet medium |
| Complex or risky code review | `reviewer` | Sol medium | Opus medium |
| Approved implementation escalation | `implementer-escalated` | Sol high | Sonnet high |

Model and effort are configured in `.codex/agents/*.toml` and
`.claude/agents/*.md`; those files, not this summary table, are authoritative.
Haiku is not part of the default Claude workflow. Escalated implementation needs
explicit user approval; Luna max is approved as a standard role.

**Routing steps:** delegate missing planning to `planner-light` or `planner`,
then the appropriate implementer. Implementers run focused tests themselves;
use `tester` only when separate execution or diagnosis is useful. Delegate
behavior-changing code review independently to `reviewer-light` or `reviewer`.
For docs-only, config-only and mechanical diffs, the coordinator can inspect
the diff. Keep the repository's existing tests, review skills, correction budget,
scope and completion requirements below. Do not rerun planning for a settled spec.
Do not create unnecessary duplicate tests or review passes.

**Model evidence:** Before first delegation, check client support and warn about
unavailable or superseded model IDs when verifiable. Do not silently substitute
models. For each phase actually run, report
`phase / role / configured model / effort / status`.
Use `configured` for config only, `spawn-confirmed` for a launched role, and
`runtime-verified` only when independent client/task metadata shows the
effective model *and* effort. An agent self-report is not evidence. Inspect
`/subagents` in Codex or `/tasks` in Claude Code if supported; note possible
runtime overrides and unavailable verification. Summarize actual versus
configured roles in the final report.

## Skills

Invoke a matching skill through the Skill tool (other harnesses: open its `SKILL.md`).

| Situation | Skill |
| --- | --- |
| Review of a ticket that changes code (`src/`, `tests/`, `sql/`) | `code-review` (independent reviewer, before commit) |
| Implementing a ticket with behavior change | `tdd` (implementer) |
| Hard bug, flaky or unexplained failure | `diagnosing-bugs` |
| A term or durable decision is being settled (glossary, ADR) | `domain-modeling` |
| Turning a settled conversation into a spec / tickets | `to-spec`, then `to-tickets` |
| Editing `AGENTS.md`, `CLAUDE.md`, agent docs or skills | `writing-for-agents` |

Skip `code-review` for docs-only, config-only or purely mechanical changes; the coordinator's
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
- Parallel implementers each use their own `CARGO_TARGET_DIR` with `CARGO_INCREMENTAL=0`
  and delete it when their ticket ends; run at most three at once. Each target directory
  takes several GB and a full disk breaks the linker.
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
