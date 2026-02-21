# EXPECTED outputs for T22637 work

These are target diagnostics for stderr baselines touched by this task.
Exact line/column spans may vary; keep core wording stable.

## testsuite/tests/rename/should_fail/T22637.stderr
- Must report conflicting inline pragmas (not duplicate).
- Expected headline:
  - `Conflicting pragmas for ‘f’`
- Test source should combine:
  - `{-# INLINE f #-}`
  - `{-# NOINLINE f #-}`

## testsuite/tests/rename/should_fail/rnfail048.stderr
- Because file mixes multiple `NOINLINE` and `INLINE` pragmas for one binder, expected category is conflict.
- Expected headline:
  - `Conflicting pragmas for ‘foo’`

## testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr
- File combines `OPAQUE` and `INLINE` for one binder, expected category is conflict.
- Expected headline:
  - `Conflicting pragmas for ‘f’`

## guardrail
- Keep true duplicates using duplicate wording (`Duplicate ... pragmas`).
