
test('space_leak_001',
     # Before trac #2747 was fixed this was 565.
     # Now it's: 3 (amd64/Linux)
     #           4 (x86/OS X)
     #           5 (x86/Linux)
     [stats_num_field('peak_megabytes_allocated', 3, 5),
      stats_num_field('max_bytes_used', 410000,
                                        450000),
                      # expected value: 440224 (amd64/Linux)
                      #                 417016 (x86/OS X)
                      #                 415672 (x86/Windows)
      if_platform('i386-unknown-mingw32',
          stats_num_field('bytes allocated', 730000000,
                                             750000000)),
                           # expected value: 739138140 (x86/Windows)
      if_not_platform('i386-unknown-mingw32',
          stats_num_field('bytes allocated', 9320000000,
                                             9350000000))
                           # expected value: 9343740368 (amd64/Linux)
                           #                 9331570416 (x86/Linux)
                           #                 9329073952 (x86/OS X)
      ],
     compile_and_run,
     [''])

test('T1969',
     [if_wordsize(32,
          compiler_stats_num_field('peak_megabytes_allocated', 15,
                                                               19)),
                                             # expected value: 15 (x86/Windows)
                                             #                 15 (x86/OS X)
                                             #                 19 (x86/OS X)
      if_wordsize(64,
          compiler_stats_num_field('peak_megabytes_allocated', 28,
                                                               32)),
                                             # expected value: 28 (amd64/Linux)
                                             # expected value: 32 (amd64/Linux)
      if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 6600000,
                                                     6800000)),
                                   # expected value: 6707308 (x86/OS X)
                                   #                 6694144 (x86/Windows)
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used', 12000000,
                                                     15000000)),
                                   # expected value: 12918232 (amd64/Linux)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 210000000,
                                                      230000000)),
                                    # expected value: 215582916 (x86/Windows)
                                    #                 221667908 (x86/OS X)
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 440000000,
                                                      480000000)),
                                    # expected value: 451976088 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile,
     [''])

