GHC Testsuite Readme
====================

For the full testsuite documentation, please see [here][1].

## Quick Guide

Commands to run testsuite:

 * Full testsuite: `make`
 * Using more threads: `make THREADS=12`
 * Reduced (fast) testsuite: `make fast`
 * Run a specific test: `make TEST=tc054`
 * Test a specific 'way': `make WAY=optllvm`
 * Keeping the run directory after test run: `make CLEANUP=0`. You will find a
   directory `{test_name}.run` in the test's source directory.
 * Test a specifc stage of GHC: `make stage=1`
 * Skip performance tests: `make SKIP_PERF_TESTS=YES`
 * Set verbosity: `make VERBOSE=n`
   where n=0: No per-test output, n=1: Only failures,
         n=2: Progress output, n=3: Include commands called (default),
         n=4: Include perf test results unconditionally,
         n=5: Echo commands in subsidiary make invocations
 * Pass in extra GHC options: `make EXTRA_HC_OPTS=-fvectorize`

You can also change directory to a specific test folder to run that
individual test or group of tests. For example:

``` .sh
$ cd tests/array
$ make
```

## Testsuite Ways

The testsuite can be run in a variety of 'ways'. This concept refers
to different ways that GHC can compile the code. For example, using
the native code generator (`-fasm`) is one way, while using the LLVM
code generator (`-fllvm`) is another way.

The various ways that GHC supports are defined in `config/ghc`

## Adding Tests

Please see the more extensive documentation [here][1].

  [1]: http://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests
