
test('haddock.base',
     [unless_in_tree_compiler(skip)
     ,if_wordsize(64,
          stats_num_field('peak_megabytes_allocated', 239,
                                                      241))
                                        # 2012-08-14: 240 (amd64/Linux)
     ,if_wordsize(64,
          stats_range_field('max_bytes_used', 87374568, 1))
                                # 2012-08-14: 87374568 (amd64/Linux)
     ,if_wordsize(64,
          stats_range_field('bytes allocated', 5920822352, 1))
                                 # 2012-08-14: 5920822352 (amd64/Linux)
      ],
     stats,
     ['../../../../libraries/base/dist-install/doc/html/base/base.haddock.t'])

test('haddock.Cabal',
     [unless_in_tree_compiler(skip)
     ,if_wordsize(64,
          stats_num_field('peak_megabytes_allocated', 201,
                                                      203))
                                        # 2012-08-14: 202 (amd64/Linux)
     ,if_wordsize(64,
          stats_range_field('max_bytes_used', 74119424, 1))
                                # 2012-08-14: 74119424 (amd64/Linux)
     ,if_wordsize(64,
          stats_range_field('bytes allocated', 3255435248, 1))
                                 # 2012-08-14: 3255435248 (amd64/Linux)
      ],
     stats,
     ['../../../../libraries/Cabal/Cabal/dist-install/doc/html/Cabal/Cabal.haddock.t'])

test('haddock.compiler',
     [unless_in_tree_compiler(skip)
     ,if_wordsize(64,
          stats_num_field('peak_megabytes_allocated', 1202,
                                                      1204))
                                        # 2012-08-14: 1203 (amd64/Linux)
     ,if_wordsize(64,
          stats_range_field('max_bytes_used', 428775544, 1))
                                # 2012-08-14: 428775544 (amd64/Linux)
     ,if_wordsize(64,
          stats_range_field('bytes allocated', 26070600504, 1))
                                 # 2012-08-14: 26070600504 (amd64/Linux)
      ],
     stats,
     ['../../../../compiler/stage2/doc/html/ghc/ghc.haddock.t'])

