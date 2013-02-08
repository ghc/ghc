
test('space_leak_001',
     # Before trac #2747 was fixed this was 565.
     # Now it's: 3 (amd64/Linux)
     #           4 (x86/OS X)
     #           5 (x86/Linux)
     [stats_range_field('peak_megabytes_allocated', 4, 1),
      stats_range_field('max_bytes_used', 440000, 10),
                        # expected value: 440224 (amd64/Linux)
                        #                 417016 (x86/OS X)
                        #                 415672 (x86/Windows)
                        #                 481456 (unreg amd64/Linux)
      stats_range_field('bytes allocated', 9079316016, 1),
                           # expected value: 9079316016 (amd64/Linux)
                           #                 9331570416 (x86/Linux)
                           #                 9329073952 (x86/OS X)
                           #                 9327959840 (x86/Windows)
      omit_ways(['profasm','profthreaded','threaded1','threaded2'])
      ],
     compile_and_run,
     [''])

test('T4334',
     # Test for a space leak in Data.List.lines (fixed with #4334)
     [extra_run_opts('1000000 2 t'),
      stats_range_field('peak_megabytes_allocated', 2, 0),
      # prof ways don't work well with +RTS -V0
      omit_ways(['profasm','profthreaded'])
      ],
     compile_and_run, [''])

test('T2762',
     [# peak_megabytes_allocated is 2 with 7.0.2.
      # Was 57 with 6.12.3.
      stats_range_field('peak_megabytes_allocated', 2, 0),
      only_ways(['normal']),
      extra_clean(['T2762A.hi', 'T2762A.o'])],
     compile_and_run, ['-O'])

test('T4018',
     [ only_ways(['optasm']), extra_run_opts('+RTS -M10m -RTS') ],
     compile_and_run, ['-fno-state-hack'])
