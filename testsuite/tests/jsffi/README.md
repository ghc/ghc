# Tests for GHC wasm backend's JavaScript FFI

This directory contains tests for JSFFI as implemented in the GHC wasm
backend. They are skipped if the target platform is not `wasm32`.
Otherwise, a recent version of `node` (at least `v21.2.0`) is required
to be in `PATH` for these tests.

## How to add a new test case

Each `testcase` is consisted of the following elements:

- A `test('testcase', ...)` statement in `all.T`, check existing
  examples there.
- A `testcase.hs` Haskell module. Since you define your own entry points and
  link with `-no-hs-main` anyway, add a `module Test where` on top to
  stop GHC from complaining about the lack of `Main.main`.
- A `testcase.stdout` standard output file. Note that we mark all
  tests as `ignore_stderr`, because `node` itself likes to emit a
  bunch of warning messages to `stderr` which changes from time to
  time and there's little added value in adding workaround for this
  issue.
- A `testcase.mjs` JavaScript ESM module. It has a default export
  function, which takes the `__exports` object that contains the wasm
  module exports as its argument. By the time this function is called
  by the test runner, the wasm instance state has been initialized,
  and you can call your exported Haskell functions here.

## How to debug when a test case goes wrong

Pass `--keep-test-files` to hadrian test to preserve the crime scene.
Go to that temporary directory and run
`/workspace/ghc/utils/jsffi/test-runner.mjs testcase.wasm +RTS ...` to
rerun the test case.
