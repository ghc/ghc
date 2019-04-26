# We do not add peak_megabytes_allocated and max_bytes_used to these tests, as
# they are somewhat unreliable, and it is harder to re-run these numbers to
# detect outliers, as described in Note [residency]. See #9556.

test('haddock.base',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',5), haddock_stats_test
      ],
     stats,
     ['base.t'])

test('haddock.Cabal',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',5), haddock_stats_test
      ],
     stats,
     ['Cabal.t'])

test('haddock.compiler',
     [unless(in_tree_compiler(), skip), req_haddock
     ,collect_stats('bytes allocated',10), haddock_stats_test
      ],
     stats,
     ['ghc.t'])
