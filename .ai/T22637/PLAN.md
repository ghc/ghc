# PLAN: GHC issue #22637

Status: mutable working plan

## Process rule
- Update this file and `.ai/T22637/REPORT.md` as work progresses.
- Step status vocabulary: `pending`, `in_progress`, `done`, `blocked`.
- Before code edits, mark exactly one step `in_progress`.
- After each meaningful change or test run, add a short entry to `REPORT.md`.
- When a step reaches `done`, create an intermediate commit if the diff is coherent.

## Command runbook
1. Baseline state:
   - `git status --short`
2. Edit + compile loop (as needed):
   - `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
3. Inspect changed files:
   - `git status --short`
   - `git diff -- .ai/T22637 compiler/GHC/Tc/Errors/Types.hs compiler/GHC/Types/Error/Codes.hs compiler/GHC/Rename/Bind.hs compiler/GHC/Tc/Errors/Ppr.hs testsuite/tests/rename/should_fail testsuite/tests/parser/should_fail`
4. Final check:
   - Ensure outputs match `.ai/T22637/EXPECTED.md`.

## Implementation plan
1. Add new diagnostic constructor and error code.
   - `compiler/GHC/Tc/Errors/Types.hs`
   - `compiler/GHC/Types/Error/Codes.hs`
   - shape: inline-specific payload (e.g. `NE.NonEmpty (LocatedN RdrName, InlinePragma GhcPs)`).
   - Status: pending

2. Split duplicate-vs-conflict reporting in renamer.
   - `compiler/GHC/Rename/Bind.hs`
   - keep existing generic duplicate handling for non-inline sigs.
   - route mixed inline kinds to new conflicting-inline diagnostic.
   - keep same-kind inline duplicates as `TcRnDuplicateSigDecl`.
   - Status: pending

3. Add pretty-printing for new diagnostic.
   - `compiler/GHC/Tc/Errors/Ppr.hs`
   - wording: `Conflicting pragmas for ‘f’` with source locations.
   - leave `TcRnDuplicateSigDecl` duplicate-focused.
   - Status: pending

4. Update diagnostics docs/comments.
   - `compiler/GHC/Tc/Errors/Types.hs`
   - duplicate examples remain true duplicates only.
   - add conflicting-inline examples under new constructor.
   - Status: pending

5. Tests.
   - add `testsuite/tests/rename/should_fail/T22637.hs`
   - add `testsuite/tests/rename/should_fail/T22637.stderr`
   - register in `testsuite/tests/rename/should_fail/all.T`
   - update:
     - `testsuite/tests/rename/should_fail/rnfail048.stderr`
     - `testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr`
   - Status: pending

6. Run targeted tests.
   - `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
   - Status: pending
