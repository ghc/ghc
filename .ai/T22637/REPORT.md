# REPORT: GHC issue #22637

## Summary
- Completed.
- Added a dedicated conflicting-inline diagnostic (`TcRnConflictingInlineSigDecl`) with an inline-specific payload.
- Kept `TcRnDuplicateSigDecl` for true duplicates only.
- Updated renamer routing, pretty-printing, and stderr baselines accordingly.

## Changed files
- `.ai/T22637/PLAN.md`
- `.ai/T22637/REPORT.md`
- `compiler/GHC/Rename/Bind.hs`
- `compiler/GHC/Tc/Errors/Ppr.hs`
- `compiler/GHC/Tc/Errors/Types.hs`
- `compiler/GHC/Types/Error/Codes.hs`
- `testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr`
- `testsuite/tests/rename/should_fail/all.T`
- `testsuite/tests/rename/should_fail/rnfail048.stderr`
- `testsuite/tests/rename/should_fail/T22637.hs`
- `testsuite/tests/rename/should_fail/T22637.stderr`

## Changes made
- Updated planning status to begin implementation step 1.
- Added `TcRnConflictingInlineSigDecl` with inline-specific payload.
- Added diagnostic code for `TcRnConflictingInlineSigDecl`.
- Updated renamer duplicate-signature reporting to emit conflicting-inline diagnostics for mixed inline kinds.
- Added dedicated pretty-printing + diagnostic metadata branches for conflicting-inline diagnostics.
- Added regression test `T22637` and registered it in rename `should_fail` tests.
- Updated stderr baselines for `rnfail048`, `OpaqueParseFail4`, and `T22637`.

## Tests run
- `hadrian/build -q -q -j3 --no-build test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"` (pass)
- `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"` (initial run: fail, `T22637.stderr` column mismatch)
- `hadrian/build -q -q -j3 --no-build test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"` (pass after fixing `T22637.stderr`)
- `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"` (pass)

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
- 2026-02-21 02:08Z | bootstrap | ran hadrian no-build targeted test command | N/A | pass
- 2026-02-21 02:08Z | baseline | checked working tree (`libraries/libffi-clib` dirty only) | N/A | noted, untouched
- 2026-02-21 02:08Z | plan | set implementation step 1 to `in_progress` | .ai/T22637/PLAN.md | updated
- 2026-02-21 02:10Z | step1 | added `TcRnConflictingInlineSigDecl` constructor docs/payload | compiler/GHC/Tc/Errors/Types.hs | done
- 2026-02-21 02:10Z | step1 | added diagnostic code mapping | compiler/GHC/Types/Error/Codes.hs | done
- 2026-02-21 02:10Z | step2 | split duplicate/conflicting inline reporting | compiler/GHC/Rename/Bind.hs | done
- 2026-02-21 02:10Z | step3 | added conflicting-inline pretty-printer + metadata branches | compiler/GHC/Tc/Errors/Ppr.hs | done
- 2026-02-21 02:10Z | plan | marked steps 1-4 done, step 5 in progress | .ai/T22637/PLAN.md | updated
- 2026-02-21 02:40Z | step5 | added `T22637` test + registered in all.T | testsuite/tests/rename/should_fail/{T22637.hs,T22637.stderr,all.T} | done
- 2026-02-21 02:40Z | step5 | updated impacted stderr baselines | testsuite/tests/rename/should_fail/rnfail048.stderr, testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr | done
- 2026-02-21 02:40Z | step6 | ran targeted tests, observed `T22637.stderr` column mismatch | .ai/T22637/hadrian-test.log | fail
- 2026-02-21 02:40Z | step5 | fixed `T22637.stderr` location columns | testsuite/tests/rename/should_fail/T22637.stderr | done
- 2026-02-21 02:40Z | step6 | reran targeted tests (`--no-build`) | N/A | pass
- 2026-02-21 02:40Z | step6 | reran targeted tests (full command) | N/A | pass
- 2026-02-21 02:40Z | plan | marked steps 5-6 done | .ai/T22637/PLAN.md | updated

## Decision log (append-only)
- 2026-02-21 | Keep duplicate and conflicting diagnostics separate.
- 2026-02-21 | Use inline-specific payload for conflicting diagnostic.
- 2026-02-21 | Regression test uses `INLINE` + `NOINLINE`.
- 2026-02-21 | Adopt `.ai/T22637/{TASK,PLAN,REPORT}.md` workflow.
- 2026-02-21 | Agents must keep PLAN/REPORT updated during execution.
- 2026-02-21 | Code style should match surrounding code in touched files.
- 2026-02-21 | Prefer intermediate logical commits over one final commit.
- 2026-02-21 | Start-of-task workflow should trigger/suggest saving Hadrian command approval.
- 2026-02-21 | Improve conflicting-inline error rendering to list all conflicting pragmas (kind + location), not just locations.

## Open risks
- None identified for the scoped diagnostics change; behavior change is covered by targeted regression tests.

## Final handoff checklist
- Final summary written.
- Changed files listed.
- Test commands + outcomes listed.
- Any remaining risks/blockers listed.
