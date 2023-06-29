GHC Testsuite Readme
====================

For the full testsuite documentation, please see [here][1].

## Quick Guide

Commands to run testsuite:

 * Full testsuite: `hadrian/build test`
 * Using more threads: `hadrian/build test -j12`
 * Reduced (fast) testsuite: `hadrian/build test --test-speed=fast`
 * Run a specific test: `hadrian/build test --only="tc055 tc054"`
 * Test a specific 'way': `hadrian/build test --test-way=optllvm`
 * Skip performance tests: `hadrian/build test --skip-perf`
 * Set verbosity: `hadrian/build test --test-verbose=<n>`
   where n=0: No per-test output, n=1: Only failures,
         n=2: Progress output, n=3: Include commands called (default),
         n=4: Include perf test results unconditionally,
         n=5: Echo commands in subsidiary make invocations


## Testsuite Ways

The testsuite can be run in a variety of 'ways'. This concept refers
to different ways that GHC can compile the code. For example, using
the native code generator (`-fasm`) is one way, while using the LLVM
code generator (`-fllvm`) is another way.

The various ways that GHC supports are defined in `config/ghc`

## Adding Tests

Please see the more extensive documentation [here][1].

  [1]: https://gitlab.haskell.org/ghc/ghc/wikis/building/running-tests
