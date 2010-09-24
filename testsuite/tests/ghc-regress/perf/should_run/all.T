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

test('T4321', omit_ways(['ghci']), compile_and_run, ['-O'])

