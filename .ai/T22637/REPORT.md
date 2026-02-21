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
  - `hadrian/build -q -q test --flavour=quickest --test-compiler=stage1 --only="rnfail048 OpaqueParseFail4 T22637"`

