# NoFib: Haskell Benchmark Suite

This is the root directory of the "NoFib Haskell benchmark suite". It
should be part of a GHC source tree, that is the 'nofib' directory
should be at the same level in the tree as 'compiler' and 'libraries'.

## Package Depedencies

Please make sure you have the following packages installed for your
system GHC:
 * html
 * regex-compat (will install: mtl, regex-base, regex-posix)

## Using

Then, to run the tests, execute:

    make clean
    make boot
    make 2>&1 | tee nofib-log

This will put the results in the file `nofib-log`. You can pass extra
options to a nofib run using the `EXTRA_HC_OPTS` variable like this:

    make clean
    make boot
    make EXTRA_HC_OPTS="-fllvm" >&1 | tee nofib-llvm-log

To compare the results of multiple runs, use the program in
`../utils/nofib-analyse`, for example:

    nofib-analyse nofib-log-6.4.2 nofib-log-6.6

to generate a comparison of the runs in captured in `nofib-log-6.4.2`
and `nofib-log-6.6`. When making comparisons, be careful to ensure
that the things that changed between the builds are only the things
that you _wanted_ to change. There are lots of variables: machine,
GHC version, GCC version, C libraries, static vs. dynamic GMP library,
build options, run options, and probably lots more. To be on the safe
side, make both runs on the same unloaded machine.

## Configuration

There are some options you might want to tweak; search for nofib in
`../mk/config.mk`, and override settings in `../mk/build.mk` as usual.

## Extra Metrics: Valgrind

To get instruction counts, memory reads/writes, and "cache misses",
you'll need to get hold of Cachegrind, which is part of
[Valgrind](http://valgrind.org).

## Extra Packages

Some benchmarks aren't run by default and require extra packages are
installed for the GHC compiler being tested. These packages include:
 * stm - for smp benchmarks

