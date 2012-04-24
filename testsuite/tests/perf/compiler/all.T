def no_core_lint(opts):
   opts.compiler_always_flags = \
       filter(lambda opt: opt != '-dcore-lint', opts.compiler_always_flags)

setTestOpts(no_core_lint)


test('T1969',
     [if_wordsize(32,
          compiler_stats_num_field('peak_megabytes_allocated', 13,
                                                               19)),
                             # expected value: 14 (x86/Windows 17/05/10)
                             #                 15 (x86/OS X)
                             #                 19 (x86/OS X)
      if_wordsize(64,
          compiler_stats_num_field('peak_megabytes_allocated', 24,
                                                               37)),
                                             # expected value: 28 (amd64/Linux)
                                             # expected value: 34 (amd64/Linux)
      if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 5000000,
                                                     7000000)),
                             # expected value: 6707308 (x86/OS X)
                             #                 5717704 (x86/Windows 17/05/10)
                             #                 6149572 (x86/Linux, 31/12/09)
      if_wordsize(64,
          compiler_stats_range_field('max_bytes_used', 12000000, 10)),
                                   # expected value: 11178376 (amd64/Linux)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 210000000,
                                                      270000000)),
                                    # expected value: 215582916 (x86/Windows)
                                    #                 221667908 (x86/OS X)
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 420000000,
                                                      580000000)),
                                    # 17/11/2009:     434,845,560 (amd64/Linux)
                                    # 08/12/2009:     459,776,680 (amd64/Linux)
                                    # 17/05/2010:     519,377,728 (amd64/Linux)
                                    # 05/08/2011:     561,382,568 (amd64/OS X)
      only_ways(['normal']),
      extra_hc_opts('-dcore-lint')
          # Leave -dcore-lint on for this one test, so that we have something
          # that will catch a regression in -dcore-lint performance.
      ],
     compile,
     [''])

# This one tests for the space leak in the native code generator,
# where it holds onto the entire asm output until the end.  The space
# leak reappears from time to time, so it's a good idea to have a test
# for it.  The residency of this test will jump by 10MB to 40MB or so
# on x86-64 if the space leak appears.

# Only run this one if we have an NCG:
if 'optasm' in config.compile_ways:
   conf_3294 = only_ways(['normal'])
else:
   conf_3294 = skip

test('T3294',
     [if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 14000000,
                                                     20000000)),
                                   # expected value: 17725476 (x86/OS X)
				   #                 14593500 (Windows)
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used', 26000000,
                                                     37000000)),
                                   # expected value: 32 478 408 (amd64/Linux)
                                   # (but varies a lot, depending on when
                                   # we GC relative to the peak).
                                   # (MCB: consistently reaching ~35 375 912 for
                                   # me lately on amd64/Darwin)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 650000000,
                                                      750000000)),
                                    # expected value: 815479800  (x86/Linux)
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 1200000000,
                                                      1500000000)),
                                    # expected value: 1357587088 (amd64/Linux)
      conf_3294
      ],
     compile,
     [''])

test('T4801',
     [ # expect_broken(5224),
       # temporarily unbroken (#5227)
      if_wordsize(32,
          compiler_stats_range_field('peak_megabytes_allocated', 30, 10)),
      if_wordsize(64, # sample from (amd64/Linux):
          compiler_stats_range_field('peak_megabytes_allocated', 47, 10)),
      # expected value: 58 (amd64/OS X):
      if_platform('x86_64-apple-darwin',
          compiler_stats_num_field('peak_megabytes_allocated', 56, 60)),
      # expected value: 228286660 (x86/OS X)
      if_wordsize(32,
          compiler_stats_range_field('bytes allocated', 185669232, 10)),
      if_wordsize(64, # (amd64/Linux):
          compiler_stats_range_field('bytes allocated', 360243576, 10)),
      # expected value: 510938976 (amd64/OS X):
      if_platform('x86_64-apple-darwin',
          compiler_stats_num_field('bytes allocated', 490000000,
                                                      530000000)),
      if_wordsize(32,
      #                    expected value: x86/OS X:  9651948
          compiler_stats_num_field('max_bytes_used',  9000000,
                                                     12000000)),
      # expected value: 18188272 (amd64/Linux)
      #                 10290952 (windows)
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used', 15000000,
                                                     20000000)),
      # expected value: 20486256 (amd64/OS X):
      if_platform('x86_64-apple-darwin',
          compiler_stats_num_field('max_bytes_used', 20000000,
                                                     23000000)),
       only_ways(['normal'])
      ],
     compile,
     [''])

test('T3064',
     [# expect_broken( 3064 ),
      # expected value: 9 (x86/Linux 30-03-2011):
      if_wordsize(32,
          compiler_stats_num_field('peak_megabytes_allocated', 7, 12)),
      # expected value: 18 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('peak_megabytes_allocated', 30, 38)),
      # expected value: 56380288 (x86/Linux) (28/6/2011)
      if_wordsize(32,
          compiler_stats_range_field('bytes allocated', 39800820, 10)),
      # expected value: 73259544 (amd64/Linux) (28/6/2011):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 200000000,
                                                      280000000)),
      # expected value: 2247016 (x86/Linux) (28/6/2011):
      if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 2000000,
                                                     3000000)),
      # expected value: 4032024 (amd64/Linux, intree) (28/6/2011):
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used', 10000000,
                                                     14000000)),
       only_ways(['normal'])
      ],
     compile,
     [''])

test('T4007',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4007'])

test('T5030',
     [# expected value: 449368924 (x86/Linux)
      if_wordsize(32,
          compiler_stats_range_field('bytes allocated', 176193448, 10)),
      # expected value: 346750856 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 300000000,
                                                      400000000)),
       only_ways(['normal'])
      ],
     compile,
     ['-fcontext-stack=300'])

test('T5631',
     [if_wordsize(32, # sample from x86/Linux
          compiler_stats_range_field('bytes allocated', 392904228, 10)),
      # expected value: 774,595,008 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 600000000,
                                                      900000000)),
       only_ways(['normal'])
      ],
     compile,
     [''])

test('parsing001',
     [# expected value: ?
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 280000000,
                                                      320000000)),
      # expected value: 587079016 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 540000000,
                                                      620000000)),
       only_ways(['normal']),
      ],
     compile_fail, [''])


test('T783',
     [ only_ways(['normal']),  # no optimisation for this one
      # expected value: 175,569,928 (x86/Linux)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 125000000,
                                                      225000000)),
      # sample: 349263216 (amd64/Linux)
      if_wordsize(64,
          compiler_stats_range_field('bytes allocated', 349263216, 10))
      ],
      compile,[''])

test('T5321Fun',
     [ only_ways(['normal']),  # no optimisation for this one
      # sample from x86/Linux
      if_wordsize(32,
          compiler_stats_range_field('bytes allocated', 341591280, 10)),
      # expected value: 669165280 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_range_field('bytes allocated', 669165280, 10))
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      # sample from x86/Linux
      if_wordsize(32,
          compiler_stats_range_field('bytes allocated', 257175456, 10)),
      # expected value: 500642456 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_range_field('bytes allocated', 500642456, 10))
      ],
      compile,[''])

test('T5642',
     [ only_ways(['normal']),
      if_wordsize(32, # sample from x86/Linux
          compiler_stats_range_field('bytes allocated', 1893427932, 10)),

      # sample: 3926235424 (amd64/Linux, 15/2/2012)
      if_wordsize(64,
          compiler_stats_range_field('bytes allocated', 3926235424, 10))
      ],
      compile,['-O'])
