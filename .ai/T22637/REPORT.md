# REPORT: GHC issue #22637

## Summary
- Not started.

## Changes made
- None yet.

## Tests run
- None yet.

## Notes / decisions
- Use separate diagnostics:
  - duplicates -> `TcRnDuplicateSigDecl`
  - mixed inline conflicts -> new conflicting-inline diagnostic
- Regression should use `INLINE` + `NOINLINE`.
- Preferred test command:
  - `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
- Optional smoke-check command:
  - `hadrian/build -q -q -j3 --no-build test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
- Workflow update: agents should update `.ai/T22637/PLAN.md` and `.ai/T22637/REPORT.md` as progress is made.

## Progress log (append-only)
- Format per entry:
  - `YYYY-MM-DD HH:MMZ | step | action | files | result`
- Keep entries short and factual.

## Decision log (append-only)
- 2026-02-21 | Keep duplicate and conflicting diagnostics separate.
- 2026-02-21 | Use inline-specific payload for conflicting diagnostic.
- 2026-02-21 | Regression test uses `INLINE` + `NOINLINE`.
- 2026-02-21 | Adopt `.ai/T22637/{TASK,PLAN,REPORT}.md` workflow.
- 2026-02-21 | Agents must keep PLAN/REPORT updated during execution.
- 2026-02-21 | Code style should match surrounding code in touched files.
- 2026-02-21 | Prefer intermediate logical commits over one final commit.
- 2026-02-21 | Start-of-task workflow should trigger/suggest saving Hadrian command approval.

## Final handoff checklist
- Final summary written.
- Changed files listed.
- Test commands + outcomes listed.
- Any remaining risks/blockers listed.
