# We do not add peak_megabytes_allocated and max_bytes_used to these tests, as
# they are somewhat unreliable, and it is harder to re-run these numbers to
# detect outliers, as described in Note [residency]. See #9556.

test('haddock.base',
     [extra_files(['../../../../libraries/base/dist-install/haddock.t']),
      unless(in_tree_compiler(), skip), req_haddock
     ,comparison('bytes allocated',5),
      ],
     stats,
     ['haddock.t'])

test('haddock.Cabal',
     [extra_files(['../../../../libraries/Cabal/Cabal/dist-install/haddock.t']),
      unless(in_tree_compiler(), skip), req_haddock
     ,comparison('bytes allocated',5),
      ],
     stats,
     ['haddock.t'])

test('haddock.compiler',
     [extra_files(['../../../../compiler/stage2/haddock.t']),
      unless(in_tree_compiler(), skip), req_haddock
     ,comparison('bytes allocated',10),
      ],
     stats,
     ['haddock.t'])
