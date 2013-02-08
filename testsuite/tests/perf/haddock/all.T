
test('haddock.base',
     [unless_in_tree_compiler(skip)
     ,when(wordsize(64),
          stats_range_field('peak_megabytes_allocated', 274, 10))
                                        # 2012-08-14: 240 (amd64/Linux)
                                        # 2012-09-18: 237 (amd64/Linux)
                                        # 2012-11-12: 249 (amd64/Linux)
                                        # 2013-01-29: 274 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('peak_megabytes_allocated', 113, 1))
                                        # 2012-08-14: 144 (x86/OSX)
                                        # 2012-10-30: 113 (x86/Windows)
     ,when(wordsize(64),
          stats_range_field('max_bytes_used', 96022312, 10))
                                # 2012-08-14: 87374568 (amd64/Linux)
                                # 2012-08-21: 86428216 (amd64/Linux)
                                # 2012-09-20: 84794136 (amd64/Linux)
                                # 2012-11-12: 87265136 (amd64/Linux)
                                # 2013-01-29: 96022312 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('max_bytes_used', 45574928, 1))
                                # 2012-08-14: 45574928 (x86/OSX)
     ,when(wordsize(64),
          stats_range_field('bytes allocated', 6064874536, 2))
                                 # 2012-08-14: 5920822352 (amd64/Linux)
                                 # 2012-09-20: 5829972376 (amd64/Linux)
                                 # 2012-10-08: 5902601224 (amd64/Linux)
                                 # 2013-01-17: 6064874536 (x86_64/Linux)
     ,when(wordsize(32),
          stats_range_field('bytes allocated', 2955470952, 1))
                                 # 2012-08-14: 3046487920 (x86/OSX)
                                 # 2012-10-30: 2955470952 (x86/Windows)
      ],
     stats,
     ['../../../../libraries/base/dist-install/doc/html/base/base.haddock.t'])

test('haddock.Cabal',
     [unless_in_tree_compiler(skip)
     ,when(wordsize(64),
          stats_range_field('peak_megabytes_allocated', 217, 10))
                             # 2012-08-14: 202 (amd64/Linux)
                             # 2012-08-29: 211 (amd64/Linux, new codegen)
                             # 2012-09-20: 227 (amd64/Linux)
                             # 2012-10-08: 217 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('peak_megabytes_allocated', 83, 1))
                                        # 2012-08-14: 116 (x86/OSX)
                                        # 2012-10-30: 83 (x86/Windows)
     ,when(wordsize(64),
          stats_range_field('max_bytes_used', 80590280, 15))
                             # 2012-08-14: 74119424 (amd64/Linux)
                             # 2012-08-29: 77992512 (amd64/Linux, new codegen)
                             # 2012-10-02: 91341568 (amd64/Linux)
                             # 2012-10-08: 80590280 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('max_bytes_used', 44224896, 5))
                             # 2012-08-14: 47461532 (x86/OSX)
                             # 2012-10-30: 44224896 (x86/Windows insreased range to 5%)
     ,when(wordsize(64),
          stats_range_field('bytes allocated', 3373401360, 2))
                             # 2012-08-14: 3255435248 (amd64/Linux)
                             # 2012-08-29: 3324606664 (amd64/Linux, new codegen)
                             # 2012-10-08: 3373401360 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('bytes allocated', 1733638168, 1))
                             # 2012-08-14: 1648610180 (x86/OSX)
                             # 2012-10-30: 1733638168 (x86/Windows)
      ],
     stats,
     ['../../../../libraries/Cabal/Cabal/dist-install/doc/html/Cabal/Cabal.haddock.t'])

test('haddock.compiler',
     [unless_in_tree_compiler(skip)
     ,when(wordsize(64),
          stats_range_field('peak_megabytes_allocated', 1240, 10))
                                        # 2012-08-14: 1203 (amd64/Linux)
                                        # 2012-08-21: 1199 (amd64/Linux)
                                        # 2012-09-20: 1228 (amd64/Linux)
                                        # 2012-10-08: 1240 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('peak_megabytes_allocated', 606, 1))
                                        # 2012-08-14: 631 (x86/OSX)
                                        # 2012-10-30: 606 (x86/Windows)
     ,when(wordsize(64),
          stats_range_field('max_bytes_used', 420105120, 10))
                                # 2012-08-14: 428775544 (amd64/Linux)
                                # 2012-09-20: 437618008 (amd64/Linux)
                                # 2012-10-08: 442768280 (amd64/Linux)
                                # 2012-11-12: 420105120 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('max_bytes_used', 220847924, 1))
                                # 2012-08-14: 231064920 (x86/OSX)
                                # 2012-10-30: 220847924 (x86/Windows)
     ,when(wordsize(64),
          stats_range_field('bytes allocated', 25990254632, 10))
                              # 2012-08-14: 26,070,600,504 (amd64/Linux)
                              # 2012-08-29: 26,353,100,288 (amd64/Linux, new CG)
                              # 2012-09-18: 26,882,813,032 (amd64/Linux)
                              # 2012-11-12: 25,990,254,632 (amd64/Linux)
     ,when(wordsize(32),
          stats_range_field('bytes allocated', 13773051312, 1))
                                 # 2012-08-14: 13471797488 (x86/OSX)
                                 # 2012-10-30: 13773051312 (x86/Windows)
      ],
     stats,
     ['../../../../compiler/stage2/doc/html/ghc/ghc.haddock.t'])

