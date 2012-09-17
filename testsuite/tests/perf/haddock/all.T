
test('haddock.base',
     [unless_in_tree_compiler(skip)
     ,if_wordsize(64,
          stats_num_field('peak_megabytes_allocated', 236,
                                                      248))
                                        # 2012-08-14: 240 (amd64/Linux)
                                        # 2012-09-18: 237 (amd64/Linux)
     ,if_wordsize(32,
          stats_num_field('peak_megabytes_allocated', 142,
                                                      143))
                                        # 2012-08-14: 144 (x86/OSX)
     ,if_wordsize(64,
          stats_range_field('max_bytes_used', 87374568, 2))
                                # 2012-08-14: 87374568 (amd64/Linux)
                                # 2012-08-21: 86428216 (amd64/Linux)
     ,if_wordsize(32,
          stats_range_field('max_bytes_used', 45574928, 1))
                                # 2012-08-14: 45574928 (x86/OSX)
     ,if_wordsize(64,
          stats_range_field('bytes allocated', 5920822352, 1))
                                 # 2012-08-14: 5920822352 (amd64/Linux)
     ,if_wordsize(32,
          stats_range_field('bytes allocated', 3046487920, 1))
                                 # 2012-08-14: 3046487920 (x86/OSX)
      ],
     stats,
     ['../../../../libraries/base/dist-install/doc/html/base/base.haddock.t'])

test('haddock.Cabal',
     [unless_in_tree_compiler(skip)
     ,if_wordsize(64,
          stats_num_field('peak_megabytes_allocated', 209,
                                                      213))
                             # 2012-08-14: 202 (amd64/Linux)
                             # 2012-08-29: 211 (amd64/Linux, new codegen)
     ,if_wordsize(32,
          stats_num_field('peak_megabytes_allocated', 115,
                                                      117))
                                        # 2012-08-14: 116 (x86/OSX)
     ,if_wordsize(64,
          stats_range_field('max_bytes_used', 77992512, 1))
                             # 2012-08-14: 74119424 (amd64/Linux)
                             # 2012-08-29: 77992512 (amd64/Linux, new codegen)
     ,if_wordsize(32,
          stats_range_field('max_bytes_used', 47461532, 1))
                             # 2012-08-14: 47461532 (x86/OSX)
     ,if_wordsize(64,
          stats_range_field('bytes allocated', 3324606664, 1))
                             # 2012-08-14: 3255435248 (amd64/Linux)
                             # 2012-08-29: 3324606664 (amd64/Linux, new codegen)
     ,if_wordsize(32,
          stats_range_field('bytes allocated', 1648610180, 1))
                             # 2012-08-14: 1648610180 (x86/OSX)
      ],
     stats,
     ['../../../../libraries/Cabal/Cabal/dist-install/doc/html/Cabal/Cabal.haddock.t'])

test('haddock.compiler',
     [unless_in_tree_compiler(skip)
     ,if_wordsize(64,
          stats_num_field('peak_megabytes_allocated', 1198,
                                                      1204))
                                        # 2012-08-14: 1203 (amd64/Linux)
                                        # 2012-08-21: 1199 (amd64/Linux)
     ,if_wordsize(32,
          stats_num_field('peak_megabytes_allocated', 630,
                                                      632))
                                        # 2012-08-14: 631 (x86/OSX)
     ,if_wordsize(64,
          stats_range_field('max_bytes_used', 428775544, 1))
                                # 2012-08-14: 428775544 (amd64/Linux)
     ,if_wordsize(32,
          stats_range_field('max_bytes_used', 231064920, 1))
                                # 2012-08-14: 231064920 (x86/OSX)
     ,if_wordsize(64,
          stats_range_field('bytes allocated', 26882813032, 1))
                              # 2012-08-14: 26,070,600,504 (amd64/Linux)
                              # 2012-08-29: 26,353,100,288 (amd64/Linux, new CG)
                              # 2012-09-18: 26,882,813,032 (amd64/Linux)
     ,if_wordsize(32,
          stats_range_field('bytes allocated', 13471797488, 1))
                                 # 2012-08-14: 13471797488 (x86/OSX)
      ],
     stats,
     ['../../../../compiler/stage2/doc/html/ghc/ghc.haddock.t'])

