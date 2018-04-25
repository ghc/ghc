Dependency Solver Benchmarks
============================

hackage-benchmark
-----------------

The goal of this benchmark is to find examples of packages that show a
difference in behavior between two versions of cabal.  It doesn't try
to determine which version of cabal performs better.

`hackage-benchmark` compares two `cabal` commands by running each one
on each package in a list.  The list is either the package index or a
list of packages provided on the command line.  In order to save time,
the benchmark initially only runs one trial for each package.  If the
results (solution, no solution, timeout, etc.) are the same and the
times are too similar, it skips the package.  Otherwise, it runs more
trials and prints the results if they are significant.
