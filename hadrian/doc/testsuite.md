# Running the testsuite

## Quickstart

The simplest way to run the testsuite is

``` sh
# assuming 'build' is the build script you're using
# (hadrian/build.sh, hadrian/build.bat, ...)
build test
```

This is the equivalent of running `make test` with the
Make build system: it will run the entire testsuite in
normal mode (as opposed to slow or fast). If you have not
built GHC before, this will also build a stage 2 GHC in
the default flavour along with many libraries and programs
needed by the tests.

## Running only a subset of the testsuite

You can use the `TEST` environment variable, like with the
Make build system, or the `--only=...` command line argument.
This is best illustrated with examples:

``` sh
# only run the test named 'sometest'
build test --only=sometest

# only run 'test1' and 'test2'
build test --only="test1 test2"

# only run 'sometest'
TEST=sometest build test

# only run 'test1' and 'test2'
TEST="test1 test2" build test

# only run 'test1', 'test2', 'test3' and 'test4'
TEST="test1 test2" build test --only="test3 test4"
```

## Performance tests

You can use the `--only-perf` and `--skip-perf` flags to
respectively run only the performance tests or everything
but the performance tests.

``` sh
# just performance tests, equivalent to:
# make test ONLY_PERF_TESTS=YES
build test --only-perf

# skip performance tests, equivalent to:
# make test SKIP_PERF_TESTS=YES
build test --skip-perf
```

## Test speed

You can run the tests in `slow`, `normal` (default) or `fast`
mode using `--test-speed={slow, normal, fast}`.

``` sh
# equivalent to: make slowtest
build test --test-speed=slow

# equivalent to: make test
build test --test-speed=normal

# equivalent to: make fasttest
build test --test-speed=fast
```

## Test ways

You can specify which test ways to use using `--test-way=<way>`,
once per way that you want to run.

``` sh
# equivalent to: make test WAY=ghci
build test --test-way=ghci

# equivalent to: make test WAY=ghci WAY=hpc
build test --test-way=ghci --test-way=hpc
```

## Misc.

```
  --summary[=TEST_SUMMARY]
```
Where to output the test summary file.

---

```
  --test-verbose[=TEST_VERBOSE]
```
A verbosity value between 0 and 5. 0 is silent, 4 and higher
activates extra output.

---

```
  --test-compiler[=TEST_COMPILER]
```
Use given compiler [Default=stage2].

---

```
  --test-config-file[=CONFIG_FILE]
```
Configuration file for testsuite. Default is
`testsuite/config/ghc`

---

```
  --config[=EXTRA_TEST_CONFIG]
```
Configurations to run test, in `key=value` format.

---

```
  --summary-junit[=TEST_SUMMARY_JUNIT]
```
Output testsuite summary in JUnit format.
