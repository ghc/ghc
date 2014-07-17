# Tests that newArray/newArray_ is being optimised correctly

# fortunately the values here are mostly independent of the wordsize,
# because the test allocates an unboxed array of doubles.

test('T3586',
     [stats_num_field('peak_megabytes_allocated', (17, 1)),
                                 # expected value: 17 (amd64/Linux)
      stats_num_field('bytes allocated', (16102024, 5)),
                        # prev:           16835544 (amd64/Linux)
                        # 2014-07-17:     16102024 (amd64/Linux), general round of updates
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T4830',
     [stats_num_field('bytes allocated',
          [(wordsize(64), 98248, 1),
           #             127000 (amd64/Linux)
           # 2013-02-07:  99264 (amd64/Linux)
           # 2014-01-13:  98248 (amd64/Linux) due to #8647
           (wordsize(32), 70646, 3)]),
           # 2013-02-10:  69744 (x86/Windows)
           # 2013-02-10:  71548 (x86/OSX)
           # 2014-01-28:  Widen range 2->3
           #                (x86/Windows - actual 69000, lower was 69233)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T3245', normal, compile_and_run, ['-O'])

# Test that bytestring reading/writing isn't over-allocating.  We had
# a bug in hGetBufNonBlocking in 6.13 that triggered this.
#
test('lazy-bs-alloc',
     [stats_num_field('peak_megabytes_allocated', (2, 1)),
                                 # expected value: 2 (amd64/Linux)
      stats_num_field('bytes allocated',
          [(wordsize(64), 425400, 1),
            #             489776 (amd64/Linux)
            # 2013-02-07: 429744 (amd64/Linux)
            # 2013-12-12: 425400 (amd64/Linux)
           (wordsize(32), 411500, 2)]),
            # 2013-02-10: 421296 (x86/Windows)
            # 2013-02-10: 414180 (x86/OSX)
            # 2014-01-22: 411500 (x86/Linux)
            # 2014-01-28: Widen 1->2% (x86/Windows was at 425212)
      only_ways(['normal']),
      extra_run_opts('../../numeric/should_run/arith011.stdout'),
      ignore_output
      ],
      # use a suitably big file, without bloating the repo with a new one:
     compile_and_run,
     ['-O'])

test('T876',
     [stats_num_field('bytes allocated',
          [(wordsize(64), 63216 , 5),
              # 2013-02-14: 1263712 (x86_64/Linux)
              # 2014-02-10:   63216 (x86_64/Linux), call arity analysis
           (wordsize(32), 53024, 5) ]),
              # some date:  663712  (Windows, 64-bit machine)
              # 2014-04-04:  56820  (Windows, 64-bit machine)
              # 2014-06-29:  53024  (x86_64/Linux)
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

test('T3736',
     extra_clean(['T3736.speed.f32']),
     run_command,
     ['$MAKE -s --no-print-directory T3736'])
test('T3738',
     [extra_clean(['T3738a.hi', 'T3738a.o']),
      stats_num_field('peak_megabytes_allocated', (1, 0)),
                                 # expected value: 1 (amd64/Linux)
      stats_num_field('bytes allocated',
                      [(wordsize(32), 45648, 5),
                    # expected value: 50520 (x86/Linux)
                       (wordsize(64), 49400, 5)]),
                    # prev:           49400 (amd64/Linux)
                    # 2014-07-17:     50520 (amd64/Linux) general round of updates
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('MethSharing',
     [stats_num_field('peak_megabytes_allocated', (1, 0)),
                                 # expected value: 1 (amd64/Linux)
      stats_num_field('bytes allocated',
                      [(wordsize(32), 360940756, 5),
                    # expected value: 2685858140 (x86/OS X)
                          # expected: 360940756 (x86/Linux)
                       (wordsize(64), 640067672, 5)]),
                          # expected: 640067672 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T2902',
     extra_clean(['T2902_A',    'T2902_B',
                  'T2902_A.hi', 'T2902_B.hi',
                  'T2902_A.o',  'T2902_B.o',
                  'T2902_A_PairingSum.hi', 'T2902_B_PairingSum.hi',
                  'T2902_A_PairingSum.o',  'T2902_B_PairingSum.o',
                  'T2902_Sum.hi',
                  'T2902_Sum.o']),
     run_command,
     ['$MAKE -s --no-print-directory T2902'])
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
                      [(wordsize(32), 47088, 5),
                    # expected value: 47088 (x86/Darwin)
                       (wordsize(64), 52600, 5)]),
                    # expected value: 51320 (amd64/Linux)
                    # 2014-07-17:     52600 (amd64/Linux) general round of updates
      only_ways(['normal', 'optasm'])
      ],
     compile_and_run,
     [''])

test('T5549',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 3362958676, 5),
                    # expected value: 3362958676 (Windows)
                       (wordsize(64), 6725846120, 5)]),
                    # expected value: 6725846120 (amd64/Linux)
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
     ignore_output,
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O'])

test('T7257',
     [stats_num_field('bytes allocated',
                      [(wordsize(32), 1150000000, 10),
                    # expected value: 1246287228 (i386/Linux)
                       (wordsize(64), 1774893760, 5)]),
                        # 2012-09-21: 1774893760 (amd64/Linux)
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
          [(wordsize(64), 60360, 1),
           #             127000 (amd64/Linux)
           # 2013-02-07:  60360 (amd64/Linux)
           (wordsize(32), 58434, 1)]),
            # 2013-02-10: 58032 (x86/Windows)
            # 2013-02-10: 58836 (x86/OSX)
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
                       (wordsize(64), 1680051336, 10)]),
              # 2014-02-10: 1680051336 (x86_64/Linux), call arity analysis
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
                      [ (wordsize(32), 50000000, 5)
                      , (wordsize(64), 95747304, 5) ]),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])
