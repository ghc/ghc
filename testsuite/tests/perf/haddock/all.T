# Note [Haddock runtime stats files]
#
# When one of the build systems builds a complete GHC distribution,
# haddock gets built and then used to generate .haddock files for each
# library. For that last step, both build systems pass an extra
# `+RTS -t<some path>.t` to record runtime statistics to the given path.
#
# Those .t files are then used by a few haddock perf tests (which all live
# under testsuite/tests/perf/haddock/). Since each build system needs to produce
# those files in different places, the testsuite driver takes the directory
# under which those files are placed as a configuration parameter,
# `config.stats_files_dir`. Each individual test then specifies the name of
# the (runtime statistics) file against which some checks are to be performed,
# in addition to declaring the test's type to be `static_stats`.

# We do not add peak_megabytes_allocated and max_bytes_used to these tests, as
# they are somewhat unreliable, and it is harder to re-run these numbers to
# detect outliers, as described in Note [Measuring residency]. See #9556.

test('haddock.base',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',5)
      ],
     static_stats,
     ['base.t'])

test('haddock.Cabal',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',5)
      ],
     static_stats,
     ['Cabal.t'])

test('haddock.compiler',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',10)
      ],
     static_stats,
     ['ghc.t'])
