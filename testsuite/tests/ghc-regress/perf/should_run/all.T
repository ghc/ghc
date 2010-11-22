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

test('T4321', omit_ways(['ghci']), compile_and_run, ['-O ' + sse2_opts])
test('T3736', normal, run_command, ['$MAKE -s --no-print-directory T3736'])
test('T3738',
     [stats_num_field('peak_megabytes_allocated', 1,
                                                  1),
                                     # expected value: 1 (amd64/Linux)
      # expected value: 12800 (x86/OS X):
      if_wordsize(32,
          stats_num_field('bytes allocated', 12700,
                                             12900)),
      if_wordsize(64,
          stats_num_field('bytes allocated', 60000,
                                             61000)),
                                     # expected value: 60368 (amd64/Linux)
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
