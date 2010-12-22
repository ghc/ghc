
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
      stats_num_field('bytes allocated', 9050000000,
                                         9100000000),
                           # expected value: 9079316016 (amd64/Linux)
                           #                 9331570416 (x86/Linux)
                           #                 9329073952 (x86/OS X)
                           #                 9327959840 (x86/Windows)
      omit_ways(['profc','profasm','profthreaded','threaded1','threaded2'])
      ],
     compile_and_run,
     [''])

test('T4334',
     # Test for a space leak in Data.List.lines (fixed with #4334)
     [extra_run_opts('1000000 2 t'),
      stats_num_field('peak_megabytes_allocated', 1, 3),
      # prof ways don't work well with +RTS -V0
      omit_ways(['profc','profasm','profthreaded'])
      ],
     compile_and_run, [''])
