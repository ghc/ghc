Tests and benchmarks
====================

Prerequisites
-------------

To run the tests and benchmarks, you will need the test data, which
you can clone from one of the following locations:

* Mercurial master repository:
  [bitbucket.org/bos/text-test-data](https://bitbucket.org/bos/text-test-data)

* Git mirror repository:
  [github.com/bos/text-test-data](https://github.com/bos/text-test-data)

You should clone that repository into the `tests` subdirectory (your
clone must be named `text-test-data` locally), then run `make -C
tests/text-test-data` to uncompress the test files.  Many tests and
benchmarks will fail if the test files are missing.

Functional tests
----------------

The functional tests are located in the `tests` subdirectory. An overview of
what's in that directory:

    Makefile          Has targets for common tasks
    Tests             Source files of the testing code
    scripts           Various utility scripts
    text-tests.cabal  Cabal file that compiles all benchmarks

The `text-tests.cabal` builds:

- A copy of the text library, sharing the source code, but exposing all internal
  modules, for testing purposes
- The different test suites

To compile, run all tests, and generate a coverage report, simply use `make`.

Benchmarks
----------

The benchmarks are located in the `benchmarks` subdirectory. An overview of
what's in that directory:

    Makefile               Has targets for common tasks
    haskell                Source files of the haskell benchmarks
    python                 Python implementations of some benchmarks
    ruby                   Ruby implementations of some benchmarks
    text-benchmarks.cabal  Cabal file which compiles all benchmarks

To compile the benchmarks, navigate to the `benchmarks` subdirectory and run
`cabal configure && cabal build`. Then, you can run the benchmarks using:

    ./dist/build/text-benchmarks/text-benchmarks

However, since there's quite a lot of benchmarks, you usually don't want to
run them all. Instead, use the `-l` flag to get a list of benchmarks:

    ./dist/build/text-benchmarks/text-benchmarks

And run the ones you want to inspect. If you want to configure the benchmarks
further, the exact parameters can be changed in `Benchmarks.hs`.
