
test('space_leak_001',
     # Before trac #2747 was fixed this was 565.
     # Now it's: 3 (amd64/Linux)
     #           4 (x86/OS X)
     #           5 (x86/Linux)
     [stats_num_field('peak_megabytes_allocated', (3, 1)),
                        # 3 (amd64/Linux, integer-gmp2)
      stats_num_field('max_bytes_used',
          [(wordsize(64), 440000, 15),
                        # 440224 (amd64/Linux)
                        # 417016 (x86/OS X)
                        # 415672 (x86/Windows)
                        # 481456 (unreg amd64/Linux)
           (wordsize(32), 428220, 10)]),
             # 2013-02-10 372072 (x86/OSX)
             # 2013-02-10 439228 (x86/OSX)
             # 2016-04-06 361400 (x86/Linux)
             # 2017-03-24 428220 (x86/Linux, 64-bit machine)

      stats_num_field('bytes allocated', 
           [ (wordsize(64), 11315747416, 5),
                        # expected value: 9079316016 (amd64/Linux)
                        #                 9331570416 (x86/Linux)
                        #                 9329073952 (x86/OS X)
                        #                 9327959840 (x86/Windows)
                        #                 11315747416 (amd64/Lnx, integer-gmp2)

             (wordsize(32), 661907800, 5),
              # 2014-12-01  13550759068 (Windows)
              # 2017-03-24  661907800   (x86/Linux, 64-bit machine) No idea
              
            ]),
      omit_ways(['profasm','profthreaded','threaded1','threaded2'])
      ],
     compile_and_run,
     [''])

test('T4334',
     # Test for a space leak in Data.List.lines (fixed with #4334)
     [extra_run_opts('1000000 2 t'),
      stats_num_field('peak_megabytes_allocated', (2, 1)),
      # prof ways don't work well with +RTS -V0
      omit_ways(['profasm','profthreaded'])
      ],
     compile_and_run, [''])

test('T2762',
     [stats_num_field('peak_megabytes_allocated', (2, 0)),
      # peak_megabytes_allocated is 2 with 7.0.2.
      # Was 57 with 6.12.3.
      # 2016-08-31:     3 (allocation area size bumped to 1MB)
      # 2017-02-22:     2 (refactor fiBind)
      only_ways(['normal']),
      extra_run_opts('+RTS -G1 -RTS' ),
      extra_clean(['T2762A.hi', 'T2762A.o'])],
     compile_and_run, ['-O'])

test('T4018',
     [ only_ways(['optasm']), extra_run_opts('+RTS -M10m -RTS') ],
     compile_and_run, ['-fno-state-hack'])

test('T4029',
     [stats_num_field('peak_megabytes_allocated',
          [(wordsize(64), 65, 10)]),
            # 2016-02-26: 66 (amd64/Linux)           INITIAL
            # 2016-05-23: 82 (amd64/Linux)           Use -G1
            # 2016-07-13: 92 (amd64/Linux)           Changes to tidyType
            # 2016-09-01: 71 (amd64/Linux)           Restore w/w limit (#11565)
            # 2017-02-12: 80 (amd64/Linux)           Type-indexed Typeable
            # 2017-02-20: 76 (amd64/Linux)           Better reading of iface files
            # 2017-03-03: 65 (amd64/Linux)           Share Typeable KindReps or more
            #                                        lazy interface file reading
      stats_num_field('max_bytes_used',
          [(wordsize(64), 18208944, 15)]),
            # 2016-02-26: 24071720 (amd64/Linux)     INITIAL
            # 2016-04-21: 25542832 (amd64/Linux)
            # 2016-05-23: 25247216 (amd64/Linux)     Use -G1
            # 2016-07-13: 27575416 (amd64/Linux)     Changes to tidyType
            # 2016-07-20: 22920616 (amd64/Linux)     Fix laziness of instance matching
            # 2016-09-01: 21648488 (amd64/Linux)     Restore w/w limit (#11565)
            # 2016-10-13: 20325248 (amd64/Linux)     Creep (downwards, yay!)
            # 2016-11-14: 21387048 (amd64/Linux)     Creep back upwards :(
            # 2017-01-18: 21670448 (amd64/Linux)     Float string literals to toplevel
            # 2017-02-07: 22770352 (amd64/Linux)     It is unclear
            # 2017-02-12: 24151096 (amd64/Linux)     Type-indexed Typeable
            # 2017-02-20: 22016200 (amd64/Linux)     Better reading of iface files
            # 2017-03-03: 19172360 (amd64/Linux)     Share Typeable KindReps or more
            #                                        lazy interface file reading
            # 2017-03-07: 20476360 (amd64/Linux)     It's not entirely clear
            # 2017-03-14: 18208944 (amd64/Darwin)    Again, not clear
            # 2017-03-15: bumped margin to 15% due to instability
      extra_hc_opts('+RTS -G1 -RTS' ),
      ],
     ghci_script,
     ['T4029.script'])
