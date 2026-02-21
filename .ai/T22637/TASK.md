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

## Test requirements
- Add a regression test using `INLINE` + `NOINLINE` for the same identifier (expected to remain illegal).
- Update existing mixed-inline stderr baselines impacted by message changes.
- Use targeted test run command:
  - `hadrian/build -q -q test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`

## Non-goals
- Do not change language legality of pragma combinations in this patch.

