# Running the testsuite

## Quickstart

The simplest way to run the testsuite is

``` sh
# assuming 'build' is the build script you're using
# (hadrian/build, hadrian/build.bat, ...)
build test
```

This is the equivalent of running `make test` with the
Make build system: it will run the entire testsuite in
normal mode (as opposed to slow or fast). If you have not
built GHC before, this will also build a stage 2 GHC in
the default flavour along with many libraries and programs
needed by the tests.

## Running only a subset of the testsuite

### Specific tests

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

### Whole directories of tests

You can also ask Hadrian to run all the tests that live under one or
more directories, under which the testsuite driver will be looking for
`.T` files (usually called `all.T`), where our tests are declared.

By default, the `test` rule tries to run all the tests available (the ones
under `testsuite/tests/` as well as all the tests of the boot libraries
or programs (`base`, `haddock`, etc).

To restrict the testsuite driver to only run a specific directory of tests,
e.g `testsuite/tests/th`, you can simply do:

``` sh
$ build -j test --test-root-dirs=testsuite/tests/th
```

If you want to run several directories of tests, you can either
use several `--test-root-dirs` arguments or just one but separating
the various directories with `:`:

``` sh
# first approach
build -j test --test-root-dirs=testsuite/tests/th --test-root-dirs=testsuite/tests/gadt

# second approach
build -j test --test-root-dirs=testsuite/tests/th:testsuite/tests/gadt
```

## Accepting new output

You can use the `-a` or `--test-accept` flag to "accept" the new
output of your tests. This has the effect of updating the expected
output of all the tests that fail due to mismatching output, so as to
consider the new output the correct one.

When the `PLATFORM` environment variable is set to `YES`, passing this flag has
the effect of accepting the new output for the current platform.

When the `OS` environment variable is set to `YES`, passing this flag has the
effect of accepting the new output for all word sizes on the current OS.

``` sh
# accept new output for all tests
build test -a

# just run and accept new output for 'test123' and 'test456'
build test -a --only="test123 test456"

# accept new output for current platform and all word sizes for
# the current OS, for all tests
PLATFORM=YES OS=YES build test -a
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

The testsuite driver will produce a summary of the observed performance metrics
if `hadrian` is passed the `--summary-metrics=<file>` flag.

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

## Considering tests to be broken

Sometimes it is necessary to mark tests as broken in a particular test
environment (e.g. a particular Linux distribution). While usually one would
want to declare this in the test definition using the `expect_broken` modifier,
this is sometimes not possible.

For these cases one can use Hadrian's `--broken-test` flag to tell the
testsuite driver to consider a test to be broken during the testsuite run.

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
