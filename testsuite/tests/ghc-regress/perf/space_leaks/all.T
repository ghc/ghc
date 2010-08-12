
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
      stats_num_field('bytes allocated', 9320000000,
                                         9350000000),
                           # expected value: 9343740368 (amd64/Linux)
                           #                 9331570416 (x86/Linux)
                           #                 9329073952 (x86/OS X)
                           #                 9327959840 (x86/Windows)
      omit_ways(['profc','profasm','profthreaded','threaded1','threaded2'])
      ],
     compile_and_run,
     [''])

