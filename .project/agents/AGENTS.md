# AI Agent Instructions

Version: 0.17.0

All paths are relative to this file. 

## File Index

Read in order before starting: `AGENTS.md` → `../tasks/todo.org` → `./repository.org`

- `./repository.org`: Repo-specific rules.
- `../tasks/todo.org`: Active task index.
- `../tasks/` & `../tasks/archive/`: Active and archived task files.
- `../tasks/template.org`: Template for new task files.
- `../../CHANGELOG.org`: Notable approved changes.

## Task States

- `TODO`: not ready.
- `PLAN`: write plan, then set `REVIEW`.
- `BUILD`: implement approved scope, then set `REVIEW`.
- `NEXT`: small or already-clear task ready for direct execution.
- `CONTINUE`: address review comments, then set `REVIEW`.
- `REVIEW`: awaiting user review.
- `CANCEL`: abandoned.
- `DONE`: completed and approved.

## General Rules

- Match user's language in chat. Use English for code, comments, docs, commits, and files.
- Org syntax: bold `*bold*`, code `~name~`, lists `-`, no manual line breaks.
- In Org source blocks, prefix lines starting with `*`, `,*`, `#+`, or `,#+` with a comma.
- Assess if the current model is appropriate. Pause and ask the user if a stronger model is needed for complexity/safety, or if a cheaper/faster model suffices.
- Small, focused changes only. No unrelated refactors.
- Do not git commit, amend, squash, merge, or change dependencies unless explicitly asked.
- Never modify secrets, `.env`, production configs, deployment credentials, `AGENTS.md`, or task templates unless explicitly instructed.
- Ask the user only for unclear scope, risky choices, or irreversible changes; otherwise make a small documented assumption and continue.

## Workflow

### 1. Prepare

1. Work only on `PLAN`, `BUILD`, `NEXT`, or `CONTINUE` tasks.
2. Create or reuse `../tasks/YYYY-MM-DD--slug.org` from `../tasks/template.org`; remove inapplicable sections.
3. Add `#+TASK_STARTED: [YYYY-MM-DD Day HH:MM]` and link the task file below the task heading in `todo.org`.
4. If on `main`, create a branch `type/description` using `feat`, `fix`, `hotfix`, `refactor`, `perf`, `docs`, `test`, `release`, `ci`, or `chore`; otherwise continue on the current branch.

### 2. PLAN Mode (status: `PLAN`)

1. Do not modify production code.
2. Create or update the active task file.
3. Write the plan under `* Planning`, following the task template. This is the canonical repo record, even if discussed in chat.
4. If higher-level instructions forbid file edits, state that in chat, do not modify files, and stop before BUILD work.
5. Set the task to `REVIEW` and notify the user. Stop.

User approves by setting `BUILD`, or requests revisions with `CONTINUE`.

### 3. BUILD Mode (status: `BUILD`)

1. Read the active task file first. Follow `* Planning` if present.
2. Read `./repository.org` for repo-specific rules.
3. Implement only the active task scope.
4. Run relevant tests and linters; 
5. Update docs/README/CHANGELOG.md only if needed.
6. Write `* Build` in the task file, following the task template.
7. Set the task to `REVIEW`.
8. Notify the user and stop. Suggested Conventional Commit message with optional body.

### 4. After Review 

If approved:

1. Set the task to `DONE`.
2. Add completion date to the `todo.org` heading and `#+TASK_COMPLETED: [YYYY-MM-DD Day HH:MM]` to the task file.
3. Preserve the full original task under `* Original Task` in the task file if needed.
4. Move the task file to `../tasks/archive/`.
5. In `todo.org`, keep only the completed heading and archived task link.

If not approved:

1. User sets `CONTINUE` or instructs AI to set it.
2. Address review comments.
3. Repeat from step 2 in `BUILD Mode`.
