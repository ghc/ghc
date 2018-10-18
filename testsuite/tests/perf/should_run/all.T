# T12791 and T5835 test that GHC uses top-level instances in places where using
# a locally given solution would produce worse code.
# See Note [Solving from instances when interacting Dicts]

test('T5835',
     [stats_num_field('max_bytes_used',
           [(wordsize(64), 44312, 10)]),
       only_ways(['normal'])
       ],
     compile_and_run,
     ['-O'])

test('T12791',
     [stats_num_field('max_bytes_used',
           [(wordsize(64), 44312, 10)]),
       only_ways(['normal'])
       ],
     compile_and_run,
     ['-O'])

# Tests that newArray/newArray_ is being optimised correctly

test('T10359',
     [stats_num_field('bytes allocated',
          [(wordsize(64), 450920, 5),
           # previously   499512    (amd64/Linux)
           # 2017-03-10   450920    (amd64/Linux)  Don't generate wrapper for !Int#
           (wordsize(32), 351508, 5)]),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T14955',
     [stats_num_field('bytes allocated',
          [(wordsize(64), 48050760, 5),
           (wordsize(32), 351508, 5)]),
      only_ways(['normal'])
      ],
     multimod_compile_and_run,
     ['T14955', '-O'])

# fortunately the values here are mostly independent of the wordsize,
# because the test allocates an unboxed array of doubles.
test('T3586',
     [stats_num_field('peak_megabytes_allocated', (17, 1)),
                                 # expected value: 17 (amd64/Linux)
      stats_num_field('bytes allocated', (16102024, 5)),
                        # prev:           16835544 (amd64/Linux)
                        # 2014-07-17:     16102024 (amd64/Linux), general round of updates
      only_ways(['normal']),

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      # Only 64-bit as we don't have a good 32-bit test environment at the moment
      when(wordsize(64), extra_hc_opts('+RTS -G1 -RTS'))
      ],
     compile_and_run,
     ['-O'])

test('T4830',
     [stats_num_field('bytes allocated',
          [(wordsize(64), 98248, 4),
           #             127000 (amd64/Linux)
           # 2013-02-07:  99264 (amd64/Linux)
           # 2014-01-13:  98248 (amd64/Linux) due to #8647
           # 2015-04-03: Widen 1->4% (amd64/Windows was doing better)
           (wordsize(32), 70646, 3)]),
           # 2013-02-10:  69744 (x86/Windows)
           # 2013-02-10:  71548 (x86/OSX)
           # 2014-01-28:  Widen range 2->3
           #                (x86/Windows - actual 69000, lower was 69233)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T3245', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, ['-O'])

# Test that bytestring reading/writing isn't over-allocating.  We had
# a bug in hGetBufNonBlocking in 6.13 that triggered this.
#
test('lazy-bs-alloc',
     [extra_files(['../../numeric/should_run/arith011.stdout']),
      stats_num_field('peak_megabytes_allocated', (2, 1)),
                                 # expected value: 2 (amd64/Linux)
      stats_num_field('bytes allocated',
          [(wordsize(64), 421792, 5),
            #             489776 (amd64/Linux)
            # 2013-02-07: 429744 (amd64/Linux)
            # 2013-12-12: 425400 (amd64/Linux)
            # 2015-04-04: Widen 1->3% (amd64/Windows was failing)
            # 2015-08-15: 431500 (Windows not good enough. avg of Windows&Linux)
            # 2015-12-15: 444720 (amd64/Linux, D1616)
            # 2015-12-17: 444720 (widen 3->5%, Windows is at 462688)
            # 2017-01-30: 421792 (amd64/Linux, strangely Type-indexed Typeable)
           (wordsize(32), 410040, 5)]),
            # 2013-02-10: 421296 (x86/Windows)
            # 2013-02-10: 414180 (x86/OSX)
            # 2014-01-22: 411500 (x86/Linux)
            # 2014-01-28: Widen 1->2% (x86/Windows was at 425212)
            # 2016-04-06: 429760 (x86/Linux) no idea what happened
            # 2017-02-14: 421448 Early inline patch
            # 2017-03-24: 410040 It's not entirely clear, widen threshold to match 64-bit case

      only_ways(['normal']),
      extra_run_opts('arith011.stdout'),
      ignore_stdout,

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      # Only 64-bit as we don't have a good 32-bit test environment at the moment
      when(wordsize(64), extra_hc_opts('+RTS -G1 -RTS'))
      ],
     # use a suitably big file, without bloating the repo with a new one:
     compile_and_run,
     ['-O'])

test('T876',
     [stats_num_field('bytes allocated',
          [(platform('x86_64-unknown-mingw32'), 53472, 5),
              # 2015-04-03: 71904 (amd64/Windows, unknown cause)
              # 2016-11-27: 66928 (amd64/Windows, unknown cause)
              # 2017-12-24: 53472 (amd64/Windows, unknown cause)

           (wordsize(64), 58128, 5),
              # 2013-02-14: 1263712 (x86_64/Linux)
              # 2014-02-10:   63216 (x86_64/Linux), call arity analysis
              # 2016-11-11:   58128 (x86_64/Linux), it's not clear

           (wordsize(32), 50408, 5) ]),
              # some date:  663712  (Windows, 64-bit machine)
              # 2014-04-04:  56820  (Windows, 64-bit machine)
              # 2014-06-29:  53024  (x86_64/Linux)
              # 2014-12-01:  56796  (Windows)
              # 2015-07-11:  53156  (x86_64/Linux)
              # 2017-03-24:  50408  (x86/Linux, 64-bit machine)

      only_ways(['normal']),
      extra_run_opts('10000')
      ],
     compile_and_run,
     ['-O'])

# Get reproducible floating-point results on x86
if config.arch == 'i386':
   sse2_opts = '-msse2'
else:
   sse2_opts = ''

test('T4321',
     omit_ways(['ghci']),
     compile_and_run, ['-O ' + sse2_opts])

test('T3736', [], run_command, ['$MAKE -s --no-print-directory T3736'])
test('T3738',
     [extra_clean(['T3738a.hi', 'T3738a.o']),
      stats_num_field('peak_megabytes_allocated', (2, 0)),
                    # expected value: 1 (amd64/Linux)
                    # 2016-08-31:     2 (allocation area size bumped to 1MB)
      stats_num_field('bytes allocated',
                      [(wordsize(32), 45648, 5),
                    # expected value: 50520 (x86/Linux)
                       (wordsize(64), 50592, 8)]),
                    # prev:           49400 (amd64/Linux)
                    # 2014-07-17:     50520 (amd64/Linux) general round of updates
                    # 2014-09-10:     50592 (amd64/Linux) post-AMP-update
                    # 2015-04-03: Widen 5->8% (amd64/Windows was doing better)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('MethSharing',
     [stats_num_field('peak_megabytes_allocated', (2, 0)),
                    # expected value: 1 (amd64/Linux)
                    # 2016-08-31:     2 (allocation area size bumped to 1MB)
      stats_num_field('bytes allocated',
                      [(wordsize(32), 240071008, 5),
                    # expected value: 2685858140 (x86/OS X)
                    # expected:       360940756 (x86/Linux)
                    # 2017-03-24:     240071008 (x86/Linux, 64-bit machine)
                       (wordsize(64), 480098192, 5)]),
                    # expected:   640067672 (amd64/Linux)
                    # 2017-01-31: 480098192 work/wrap noinline things
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T2902', [], run_command, ['$MAKE -s --no-print-directory T2902'])
test('T149',
     [ # expect_broken(149),
       # working (2 Jul 2013, x86-64/Linux)
      extra_clean(['T149_A',    'T149_B',
                   'T149_A.hi', 'T149_B.hi',
                   'T149_A.o',  'T149_B.o'])],
     run_command,
     ['$MAKE -s --no-print-directory T149'])

test('T5113',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 4000000, 5),
                       (wordsize(64), 8000000, 5)]),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])


test('T4978',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 10000000, 5),
                       (wordsize(64), 10137680, 5)]),
                    # expected value: 10137680 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T5205',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 49460, 5),
                    # expected value: 47088 (x86/Darwin)
                    # 2017-03-24:     49460 (x86/Linux, 64-bit machine)

                       (platform('x86_64-unknown-mingw32'), 52264, 5),
                    # 2016-12-14: 52264 (Separate out Windows results)

                       (wordsize(64), 56208, 5)]),
                    # expected value: 51320 (amd64/Linux)
                    # 2014-07-17:     52600 (amd64/Linux) general round of updates
                    # 2015-04-03: Widen 5->7% (amd64/Windows was doing better)
                    # 2015-08-15: 50648 (Windows too good. avg of Windows&Linux)
                    # 2015-10-30: 56208 (D757: Emit Typeable at definition site)
                    # 2016-12-14: Narrow 7->5% (Separate out Windows results)
      only_ways(['normal', 'optasm'])
      ],
     compile_and_run,
     [''])

test('T5549',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 2896607976, 5),
                    # expected value: 3362958676 (Windows)
                    # 2014-12-01:     4096606332 (Windows) integer-gmp2
                    # 2017-03-24:     2896607976 (x86/Linux, 64-bit machine)

                       (wordsize(64), 5793140200, 5)]),
                    # expected value: 6725846120 (amd64/Linux)
                    #                 8193140752 (amd64/Linux) integer-gmp2
                    #                 5793140200 (amd64/Linux) integer-gmp2
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T4474a',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 2405242767, 5),
                       (wordsize(64), 4831890304, 5)]),
                    # expected value: 4831890304 (amd64/OSX)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474b',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 2405242767, 5),
                       (wordsize(64), 4831890304, 5)]),
                    # expected value: 4831890304 (amd64/OSX)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474c',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 2405242767, 5),
                       (wordsize(64), 4831890304, 5)]),
                    # expected value: 4831890304 (amd64/OSX)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T5237',
     [stats_num_field('bytes allocated',
                        [(platform('i386-unknown-mingw32'), 73280, 5),
                         (wordsize(32), 78328, 5),
                      # expected value: 78328 (i386/Linux)
                         (wordsize(64), 104176, 5)]),
                      # expected value: 110888 (amd64/Linux)
                      # expected value: 104176 (amd64/Linux)
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O ' + sse2_opts])

test('T5536',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 446260520, 1),
                                   # 1246287228 (i386/Linux)
                                    # 446328556 (i386/Windows)
                                    # 446192484 (i386/OSX)
                       (wordsize(64), 892399040, 5)]),
                   # expected value: 2492589480 (amd64/Linux)
                   # 17/1/13:         892399040 (x86_64/Linux)
                   #                  (new demand analyser)
     extra_clean(['T5536.data']),
     ignore_stdout,
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O'])

test('T7257',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 869850704, 10),
                        # expected value: 1246287228 (i386/Linux)
                        # 2016-04-06: 989850664 (i386/Linux) no idea what happened
                        # 2017-03-25: 869850704 (x86/Linux, 64-bit machine) probably sizeExpr fix
                       (wordsize(64), 1414893248, 5)]),
                        # 2012-09-21: 1774893760 (amd64/Linux)
                        # 2015-11-03: 1654893248 (amd64/Linux)
                        # 2016-06-22: 1414893248 (amd64/Linux, sizeExpr fix)
      stats_num_field('peak_megabytes_allocated',
                      [(wordsize(32), 217, 5),
                        # 2012-10-08: 217 (x86/Linux)
                       (wordsize(64), 227, 5)]),
                        # 2012-09-21: 227 (amd64/Linux)

      only_ways(['normal'])
     ],
    compile_and_run, ['-O'])

test('Conversions',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 76768, 3),
                        # 2012-12-18: 55316 Guessed 64-bit value / 2
                        # 2013-02-10: 77472 (x86/OSX)
                        # 2013-02-10: 79276 (x86/Windows)
                        # 2014-01-13: 76768 (x86/Linux) due to #8647
                       (wordsize(64), 107544, 5)]),
                        # 2012-12-18: 109608 (amd64/OS X)
                        # 2014-07-17: 107544 (amd64/Linux)

      only_ways(['normal'])
     ],
    compile_and_run, ['-O -msse2'])

test('T7507', omit_ways(['ghci']), compile_and_run, ['-O'])
# For 7507, stack overflow is the bad case

test('T7436',
     [stats_num_field('max_bytes_used',
          [(wordsize(64), 60360, 4),
           #             127000 (amd64/Linux)
           # 2013-02-07:  60360 (amd64/Linux)
           # 2015-04-03: Widen 1->4% (amd64/Windows was doing better)
           (wordsize(32), 42772, 4)]),
           # 2013-02-10: 58032 (x86/Windows)
           # 2013-02-10: 58836 (x86/OSX)
           # 2017-03-24: 42772 (x86/Linux, 64-bit machine) no idea why
           # 2017-04-02: Widen 1->4% (i386/Windows was doing better)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7797',
      [stats_num_field('bytes allocated',
                      [(wordsize(32), 240044984, 5),
                          # expected value: 2685858140 (x86/OS X)
                          # expected: 360940756 (x86/Linux)
                          # expected: 240044984 (x86/Windows, 64bit machine)
                       (wordsize(64), 480050944, 5)]),
                          # expected: 480050944 (amd64/Linux)
      extra_clean(['T7797a.hi', 'T7797a.o']),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7954',
      [stats_num_field('bytes allocated',
                      [(wordsize(32), 920045264, 10),
              # some date:  1380051408    (64-bit Windows machine)
              # 2014-04-04:  920045264    (64-bit Windows machine)
                       (wordsize(64), 1280051632, 10)]),
              # 2014-02-10: 1680051336 (x86_64/Linux), call arity analysis
              # 2018-05-03: 1280051632 (x86_64/Linux), refactor numericEnumFrom
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7850',
     [stats_num_field('peak_megabytes_allocated',
                      [(wordsize(32), 2, 10),
                       (wordsize(64), 4, 10)]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T5949',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 116020, 10),
                        (wordsize(64), 201008, 10)]),
                      # previously, it was >400000 bytes
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T4267',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 36012, 10)
                      # 32-bit value close to 64 bit; c.f. T7619
                      , (wordsize(64), 40992, 10) ]),
                      # previously, it was >170000 bytes
                      # 2014-01-17: 130000
                      # 2014-02-10: 40992 (x86_64/Linux), call arity analysis
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T7619',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 36012, 10)
                      # 32-bit close to 64-bit value; most of this very
                      # small number is standard start-up boilerplate I think
                      , (wordsize(64), 40992, 10) ]),
                      # previously, it was >400000 bytes
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('InlineArrayAlloc',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 800040960, 5)
                      , (wordsize(64), 1600040960, 5) ]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('InlineByteArrayAlloc',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 1360036012, 5)
                      , (wordsize(64), 1440040960, 5) ]),
         # 32 and 64 bit not so different, because
         # we are allocating *byte* arrays
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('InlineCloneArrayAlloc',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 800041120, 5)
                      , (wordsize(64), 1600041120, 5) ]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T9203',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32), 77969268, 5)
                      # was
                      # 2016-04-06     84345136 (i386/Debian) not sure
                      # 2017-03-24     77969268 (x86/Linux, 64-bit machine) probably join points

                      , (wordsize(64), 84620888, 5) ]),
                      # was            95747304
                      # 2019-09-10     94547280 post-AMP cleanup
                      # 2015-10-28     95451192 emit Typeable at definition site
                      # 2016-12-19     84620888 Join points
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T9339',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32),    46904, 5)
                      # is this number correct? Seems very high.
                      # 2017-03-24:       46904 (x86/Linux, 64-bit machine) who knows

                      , (platform('x86_64-unknown-mingw32'), 47088, 7)
                      # 2017-02-19                           47088 (x64/Windows) - Unknown

                      , (wordsize(64),       50728, 5) ]),
                      # w/o fusing last: 320005080
                      # 2014-07-22:       80050760
                      # 2016-08-17:          50728 Join points (#12988)
      only_ways(['normal'])],
     compile_and_run,
     ['-O2 -fspec-constr-keen'])
     # For the -fspec-constr-keen see Note [Making SpecConstr keener] in SpecConstr


test('T8472',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32),    50000, 80)
                      , (wordsize(64),    51424, 80) ]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T12996',
     [stats_num_field('bytes allocated',
                      [ (wordsize(64),    76776, 5) ]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T13001',
     [stats_num_field('bytes allocated',
                      [ (wordsize(32),    46728, 20)
                      , (wordsize(64),    50600, 20) ]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T12990',
    [stats_num_field('bytes allocated',
                     [ (wordsize(64), 20040936, 5) ]),
                     # 2017-01-03     34440936  w/o inlining unsaturated
                     #                          constructor wrappers
                     # 2017-01-03     21640904 inline wrappers
                     # 2017-01-31     20040936 work/wrap noinline things
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T13218',
    [stats_num_field('bytes allocated',
                     [ (wordsize(64), 82040056, 5) ]),
                     # 8.1 with default <$  163644216
                     # 8.1 with derived <$   82040056
     stats_num_field('max_bytes_used',
                     [ (wordsize(64), 359128, 10) ]),
                     # 8.1 with default <$  64408248
                     # 8.1 with derived <$    359128
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('DeriveNull',
    [stats_num_field('bytes allocated',
                    [ (wordsize(64), 112050856, 5) ]),
                    # 2017-04-01     152083704 w/o derived null
                    # 2017-04-02     112050856 derive null
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('DeriveNullTermination', normal, compile_and_run, [''])

test('T13623',
    [stats_num_field('bytes allocated',
                    [(platform('x86_64-unknown-mingw32'),   47232, 10),
                    # 2017-12-24     47232 unknown
                    (wordsize(64), 50936, 5)]),
                    # 2017-05-02     50936 initial
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T14052',
     [stats_num_field('bytes allocated',
                      [ (wordsize(64), 2346183840, 15) ])],
     ghci_script,
     ['T14052.script'])

test('T14936',
     [stats_num_field('bytes allocated',
                      [(platform('x86_64-unknown-mingw32'),   47536, 10),
                       # 2018-05-04     47536 unknown
                       (wordsize(64), 51792, 5) ])],
     compile_and_run,
     ['-O2'])
