# TASK: GHC issue #22637

Issue: https://gitlab.haskell.org/ghc/ghc/-/issues/22637

## Immutability
This file is the task contract and should not be edited during implementation unless the user explicitly changes requirements.

## Objective
Fix misleading diagnostics for conflicting inline pragmas.

## Required behavior
- Keep `TcRnDuplicateSigDecl` for true duplicate signatures only.
- Introduce a new diagnostic for conflicting inline pragmas (mixed inline kinds on the same binder).
- The new conflicting-inline diagnostic should use an inline-specific payload type, not generic `Sig`.
- Preserve current behavior for non-inline duplicate signatures.
- Use constructor name `TcRnConflictingInlineSigDecl`.
- Use payload type:
  - `NE.NonEmpty (LocatedN RdrName, InlinePragma GhcPs)`
- Render message heading as:
  - `Conflicting pragmas for ‘<name>’`
- Render conflicting-inline details by listing all conflicting pragmas
  (pragma kind + source location), not locations alone.
- Keep `TcRnDuplicateSigDecl` wording duplicate-focused.

## Test requirements
- Add a regression test using `INLINE` + `NOINLINE` for the same identifier (expected to remain illegal).
- Update existing mixed-inline stderr baselines impacted by message changes.
- Align stderr text with `.ai/T22637/EXPECTED.md`.
- Use targeted test run command:
  - `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`

## Non-goals
- Do not change language legality of pragma combinations in this patch.

## Workflow requirement
- As implementation progresses, agents must keep `.ai/T22637/PLAN.md` and `.ai/T22637/REPORT.md` up to date.
- Code style requirement: fit in with surrounding code style and local conventions in each edited file.
- At task start, trigger command-permission approval for the Hadrian test prefix before doing implementation work.

## Workspace constraints
- There is an unrelated dirty workspace entry at `libraries/libffi-clib`.
- Do not modify, stage, revert, or include `libraries/libffi-clib` in T22637 commits.

## Definition Of Done
- Build compiles after code edits relevant to diagnostics.
- New constructor `TcRnConflictingInlineSigDecl` exists with inline-specific payload.
- Renamer emits:
  - `TcRnDuplicateSigDecl` for true duplicates.
  - `TcRnConflictingInlineSigDecl` for mixed inline kinds.
- Pretty-printer has dedicated branch for `TcRnConflictingInlineSigDecl`.
- Conflicting-inline pretty-printer output includes every conflicting pragma
  with kind and location.
- Tests updated:
  - `testsuite/tests/rename/should_fail/T22637.{hs,stderr}`
  - `testsuite/tests/rename/should_fail/rnfail048.stderr`
  - `testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr`
- Targeted test command passes with `--test-compiler=stage1`.
- `.ai/T22637/REPORT.md` contains final summary, changed files, tests run, and open risks (if any).

## Failure / rollback guidance
- If edits break unrelated diagnostics, narrow pattern matches to inline-specific paths only.
- If expected stderr churn is larger than intended, stop and document exact extra diffs in `REPORT.md`.
- Do not alter pragma legality rules as a workaround.

## Commit policy
- Use incremental, descriptive commits.
- Commit intermediate logical milestones; do not wait for a single end-of-task commit.
- Keep each commit focused (one coherent change set).
- Suggested format:
  - `T22637: add conflicting inline pragma diagnostic`
  - `T22637: add regression and update stderr baselines`
