# Tests that newArray/newArray_ is being optimised correctly

# fortunately the values here are mostly independent of the wordsize,
# because the test allocates an unboxed array of doubles.

test('3586',
     [stats_num_field('peak_megabytes_allocated', 17,
                                                  18),
                                     # expected value: 17 (amd64/Linux)
      stats_num_field('bytes allocated', 16000000,
                                         17000000),
                                     # expected value: 16835544 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T4830',
     [stats_num_field('bytes allocated', 60000,
                                         200000),
                                     # expected value: 127,000 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T3245', normal, compile_and_run, ['-O'])

# Test that bytestring reading/writing isn't over-allocating.  We had
# a bug in hGetBufNonBlocking in 6.13 that triggered this.
#
test('lazy-bs-alloc',
     [stats_num_field('peak_megabytes_allocated', 1,
                                                  3),
                                     # expected value: 2 (amd64/Linux)
      stats_num_field('bytes allocated', 400000,
                                         600000),
                                     # expected value: 489776 (amd64/Linux)
      only_ways(['normal']),
      extra_run_opts('../../numeric/should_run/arith011.stdout'),
      ignore_output
      ],
      # use a suitably big file, without bloating the repo with a new one:
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
      stats_num_field('peak_megabytes_allocated', 1,
                                                  1),
                                     # expected value: 1 (amd64/Linux)
      # expected value: 45648 (x86/Linux):
      if_wordsize(32,
          stats_num_field('bytes allocated', 40000,
                                             50000)),
      if_wordsize(64,
          stats_num_field('bytes allocated', 40000,
                                             60000)),
                                     # expected value: 49400 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('MethSharing',
     [stats_num_field('peak_megabytes_allocated', 1,
                                                  1),
                                     # expected value: 1 (amd64/Linux)
      # expected value: 2685858140 (x86/OS X):
      if_wordsize(32,
          stats_num_field('bytes allocated', 300000000,
                                             400000000)),
                                  # expected: 360940756 (x86/Linux)
      if_wordsize(64,
          stats_num_field('bytes allocated', 600000000,
                                             700000000)),
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
     [expect_broken(149),
      extra_clean(['T149_A',    'T149_B',
                   'T149_A.hi', 'T149_B.hi',
                   'T149_A.o',  'T149_B.o'])],
     run_command,
     ['$MAKE -s --no-print-directory T149'])

test('T5113',
     [
      if_wordsize(32,
          stats_num_field('bytes allocated', 3000000,
                                             5000000)),
      if_wordsize(64,
          stats_num_field('bytes allocated', 8000000,
                                             9000000)),
      only_ways(['normal']),
      expect_broken(7046)
      ],
     compile_and_run,
     ['-O'])


test('T4978',
     [if_wordsize(32,
          stats_num_field('bytes allocated',  9000000,
                                             11000000)),
      if_wordsize(64,
          stats_num_field('bytes allocated',  9000000,
                                             11000000)),
                                     # expected value: 10137680 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T5205',
     [if_wordsize(32,
          stats_num_field('bytes allocated', 40000,
                                             50000)),
                           # expected value: 47088 (x86/Darwin)
      if_wordsize(64,
          stats_num_field('bytes allocated', 40000,
                                             60000)),
                           # expected value: 51320 (amd64/Linux)
      only_ways(['normal', 'optasm'])
      ],
     compile_and_run,
     [''])

test('T5549',
     [if_wordsize(32,
          stats_num_field('bytes allocated', 3000000000,
                                             8000000000)),
                           # expected value: 3362958676 (Windows)
      if_wordsize(64,
          stats_num_field('bytes allocated', 5000000000,
                                             8000000000)),
                           # expected value: 6,725,846,120 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T4474a',
     [if_wordsize(32,
          stats_num_field('bytes allocated', 1600000000,
                                             2000000000)),
                           # expected value: 1879095912 (i386/OSX)
      if_wordsize(64,
          stats_num_field('bytes allocated', 3500000000,
                                             3900000000)),
                           # expected value: 3766493912 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474b',
     [if_wordsize(32,
          stats_num_field('bytes allocated', 1600000000,
                                             2000000000)),
                           # expected value: 1879095912 (i386/OSX)
      if_wordsize(64,
          stats_num_field('bytes allocated', 3500000000,
                                             3900000000)),
                           # expected value: 3766493912 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474c',
     [if_wordsize(32,
          stats_num_field('bytes allocated', 1600000000,
                                             2000000000)),
                           # expected value: 1879095912 (i386/OSX)
      if_wordsize(64,
          stats_num_field('bytes allocated', 3500000000,
                                             3900000000)),
                           # expected value: 3766493912 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T5237',
     [if_wordsize(32,
          stats_num_field('bytes allocated',  70000,
                                              90000)),
                           # expected value: 78328 (i386/Linux)
      if_wordsize(64,
          stats_num_field('bytes allocated',  90000,
                                             130000)),
                           # expected value: 110888 (amd64/Linux)
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O ' + sse2_opts])

test('T5536',
     [if_wordsize(32,
          stats_num_field('bytes allocated', 1150000000,
                                             1250000000)),
                           # expected value: 1246287228 (i386/Linux)
      if_wordsize(64,
          stats_range_field('bytes allocated', 892399040, 5)),
                           # expected value: 2,492,589,480 (amd64/Linux)
                           # 17/1/13:          892,399,040 (x86_64/Linux)
                           #                   (new demand analyser)
     extra_clean(['T5536.data']),
     ignore_output,
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O'])

test('T7257',
     [if_wordsize(32,
          stats_range_field('bytes allocated', 1150000000, 10)),
                           # expected value: 1246287228 (i386/Linux)
      if_wordsize(32,
          stats_range_field('peak_megabytes_allocated', 217, 5)),
                           # 2012-10-08: 217 (x86/Linux)
      if_wordsize(64,
          stats_range_field('bytes allocated', 1774893760, 5)),
                           # 2012-09-21: 1774893760 (amd64/Linux)
      if_wordsize(64,
          stats_range_field('peak_megabytes_allocated', 227, 5)),
                           # 2012-09-21: 227 (amd64/Linux)

      only_ways(['normal'])
     ],
    compile_and_run, ['-O'])

test('Conversions',
     [if_wordsize(32,
          stats_range_field('bytes allocated', 55316, 5)),
                           # 2012-12-18: Guessed 64-bit value / 2
      if_wordsize(64,
          stats_range_field('bytes allocated', 110632, 5)),
                           # 2012-12-18: 109608 (amd64/OS X)

      only_ways(['normal'])
     ],
    compile_and_run, ['-O'])

test('T7507', omit_ways(['ghci']), compile_and_run, ['-O'])
# For 7507, stack overflow is the bad case

test('T7436',
     [stats_num_field('max_bytes_used', 50000,
                                         100000),
                                     # expected value: 127,000 (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
