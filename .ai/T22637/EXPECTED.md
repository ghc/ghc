# EXPECTED outputs for T22637 work

These are target diagnostics for stderr baselines touched by this task.
Exact line/column spans may vary; keep core wording stable.

## flag-dependent rendering
- With `-fno-diagnostics-show-caret`:
  - include headline + `at ...` locations only
  - do not include source excerpt lines
- With `-fdiagnostics-show-caret`:
  - include source excerpt lines (`|` block) containing conflicting pragma lines
  - do **not** include a caret marker line (`^`)

## testsuite/tests/rename/should_fail/T22637.stderr
- Must report conflicting inline pragmas (not duplicate).
- Expected headline:
  - `Conflicting pragmas for ‘f’`
- Expected conflicting-detail style:
  - keep an `at ...` location block
  - then include a source excerpt block starting with `|`
  - include conflicting pragma lines as line-numbered source lines, e.g.
    - `6 | {-# INLINE f #-}`
    - `7 | {-# NOINLINE f #-}`
  - no caret marker line (`^`)
  - do **not** use a `Pragmas:` summary list
- Test source should combine:
  - `{-# INLINE f #-}`
  - `{-# NOINLINE f #-}`

## testsuite/tests/rename/should_fail/rnfail048.stderr
- Because file mixes multiple `NOINLINE` and `INLINE` pragmas for one binder, expected category is conflict.
- Expected headline:
  - `Conflicting pragmas for ‘foo’`
- Expected conflicting-detail style:
  - keep an `at ...` location block
  - include a source excerpt block starting with `|`
  - include each conflicting pragma as a line-numbered source line
  - no caret marker line (`^`)
  - do **not** use a `Pragmas:` summary list

## testsuite/tests/parser/should_fail/OpaqueParseFail4.stderr
- File combines `OPAQUE` and `INLINE` for one binder, expected category is conflict.
- Expected headline:
  - `Conflicting pragmas for ‘f’`
- Expected conflicting-detail style:
  - keep an `at ...` location block
  - no source excerpt block (this test currently runs with `-fno-diagnostics-show-caret`)
  - no caret marker line (`^`)
  - do **not** use a `Pragmas:` summary list

## guardrail
- Keep true duplicates using duplicate wording (`Duplicate ... pragmas`).
