# PLAN: GHC issue #22637

Status: mutable working plan

## Process rule
- Update this file and `.ai/T22637/REPORT.md` as work progresses.
- Step status vocabulary: `pending`, `in_progress`, `done`, `blocked`.
- Before code edits, mark exactly one step `in_progress`.
- After each meaningful change or test run, add a short entry to `REPORT.md`.
- When a step reaches `done`, create an intermediate commit if the diff is coherent.
- If command output must be redirected to a log file, write logs under `/tmp` (for example: `/tmp/T22637-<name>.log`).

## Command runbook
1. Permission bootstrap (first action):
   - Trigger approval request using:
     - `hadrian/build -q -q -j3 --no-build test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
   - Ask user to save approval rule for future sessions with prefix:
     - `["hadrian/build","-q","-q","-j3","test"]`
2. Baseline state:
   - `git status --short`
3. Optional smoke-check command shape:
   - `hadrian/build -q -q -j3 --no-build test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
4. Edit + compile loop (as needed):
   - `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
5. Inspect changed files:
   - `git status --short`
   - `git diff -- .ai/T22637 compiler/GHC/Tc/Errors/Types.hs compiler/GHC/Types/Error/Codes.hs compiler/GHC/Rename/Bind.hs compiler/GHC/Tc/Errors/Ppr.hs testsuite/tests/rename/should_fail testsuite/tests/parser/should_fail`
6. Final check:
   - Ensure outputs match `.ai/T22637/EXPECTED.md`.

## Implementation plan
1. Add new diagnostic constructor and error code.
   - `compiler/GHC/Tc/Errors/Types.hs`
   - `compiler/GHC/Types/Error/Codes.hs`
   - shape: `Sig` payload (e.g. `NE.NonEmpty (LocatedN RdrName, Sig GhcPs)`).
   - Status: done

2. Split duplicate-vs-conflict reporting in renamer.
   - `compiler/GHC/Rename/Bind.hs`
   - keep existing generic duplicate handling for non-inline sigs.
   - route mixed inline kinds to new conflicting-inline diagnostic.
   - keep same-kind inline duplicates as `TcRnDuplicateSigDecl`.
   - Status: done

3. Add pretty-printing for new diagnostic.
   - `compiler/GHC/Tc/Errors/Ppr.hs`
   - wording: `Conflicting pragmas for ‘f’` with source locations.
   - leave `TcRnDuplicateSigDecl` duplicate-focused.
   - Status: done

4. Update diagnostics docs/comments.
   - `compiler/GHC/Tc/Errors/Types.hs`
   - duplicate examples remain true duplicates only.
   - add conflicting-inline examples under new constructor.
   - Status: done

5. Tests.
   - add `testsuite/tests/rename/should_fail/T22637.hs`
   - add `testsuite/tests/rename/should_fail/T22637.stderr`
   - register in `testsuite/tests/rename/should_fail/all.T`
   - update:
     - `testsuite/tests/rename/should_fail/rnfail048.stderr`
     - `testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr`
   - Status: done

6. Run targeted tests.
  - `hadrian/build -q -q -j3 test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`
  - Status: done

7. Improve conflicting-inline diagnostic details.
   - `compiler/GHC/Tc/Errors/Ppr.hs`
   - list each conflicting inline pragma kind with its source location.
   - update affected stderr baselines.
   - Status: done

8. Render conflicting pragmas with issue-#22637 source-excerpt style.
   - `compiler/GHC/Tc/Errors/Ppr.hs`
   - replace `Pragmas:` kind/location list with the issue-#22637 format:
     - keep `at ...` locations
     - then print source excerpt lines under `|`, one per conflicting pragma.
     - when `-fdiagnostics-show-caret` is enabled, caret marker output is allowed.
   - update affected stderr baselines:
     - `testsuite/tests/rename/should_fail/T22637.stderr`
     - `testsuite/tests/rename/should_fail/rnfail048.stderr`
     - `testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr`
   - Status: in_progress

9. Switch conflicting-inline diagnostic payload from `InlinePragma` to `Sig`.
   - `compiler/GHC/Tc/Errors/Types.hs`
   - `compiler/GHC/Rename/Bind.hs`
   - `compiler/GHC/Tc/Errors/Ppr.hs`
   - keep same rendered message style and behavior.
   - Status: pending
