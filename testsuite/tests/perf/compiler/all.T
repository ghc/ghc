test('T1969',
     [if_wordsize(32,
          compiler_stats_num_field('peak_megabytes_allocated', 13,
                                                               19)),
                             # expected value: 14 (x86/Windows 17/05/10)
                             #                 15 (x86/OS X)
                             #                 19 (x86/OS X)
      if_wordsize(64,
          compiler_stats_num_field('peak_megabytes_allocated', 24,
                                                               32)),
                                             # expected value: 28 (amd64/Linux)
                                             # expected value: 32 (amd64/Linux)
      if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 5000000,
                                                     7000000)),
                             # expected value: 6707308 (x86/OS X)
                             #                 5717704 (x86/Windows 17/05/10)
                             #                 6149572 (x86/Linux, 31/12/09)
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used',  9000000,
                                                     13000000)),
                                   # expected value: 11404664 (amd64/Linux)
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
      only_ways(['normal'])
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
          compiler_stats_num_field('max_bytes_used', 15000000,
                                                     20000000)),
                                   # expected value: 17725476 (x86/OS X)
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
      # expected value: 11 (x86/OSX):
      if_wordsize(32,
          compiler_stats_num_field('peak_megabytes_allocated', 30, 40)),
      # expected value: 52 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('peak_megabytes_allocated', 50, 54)),
      # expected value: 64 (amd64/OS X):
      if_platform('x86_64-apple-darwin',
          compiler_stats_num_field('peak_megabytes_allocated', 62, 66)),
      # expected value: 228286660 (x86/OS X)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 200000000,
                                                      240000000)),
      # expected value: 458700632 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 440000000,
                                                      480000000)),
      # expected value: 510938976 (amd64/OS X):
      if_platform('x86_64-apple-darwin',
          compiler_stats_num_field('bytes allocated', 490000000,
                                                      530000000)),
      # expected value: 14181360 (x86/Linux)
      if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 11000000,
                                                     14000000)),
      # expected value: 18188272 (amd64/Linux)
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used', 15000000,
                                                     20000000)),
      # expected value: 23097936 (amd64/OS X):
      if_platform('x86_64-apple-darwin',
          compiler_stats_num_field('max_bytes_used', 21000000,
                                                     25000000)),
       only_ways(['normal'])
      ],
     compile,
     [''])

test('T3064',
     [
      # expected value: 9 (x86/Linux 30-03-2011):
      if_wordsize(32,
          compiler_stats_num_field('peak_megabytes_allocated', 8, 11)),
      # expected value: 18 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('peak_megabytes_allocated', 9, 16)),
      # expected value: 56380288 (x86/Linux) (28/6/2011)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 50000000,
                                                      70000000)),
      # expected value: 108937496 (amd64/Linux) (28/6/2011):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 110000000,
                                                      140000000)),
      # expected value: 2247016 (x86/Linux) (28/6/2011):
      if_wordsize(32,
          compiler_stats_num_field('max_bytes_used', 2000000,
                                                     3000000)),
      # expected value: 4032024 (amd64/Linux, intree) (28/6/2011):
      if_wordsize(64,
          compiler_stats_num_field('max_bytes_used', 3000000,
                                                     5000000)),
       only_ways(['normal'])
      ],
     compile,
     [''])

test('T4007',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4007'])

test('T5030',
     [# expected value: 629864032 (x86/Darwin)
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 600000000,
                                                      650000000)),
      # expected value: 1255998208 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 1200000000,
                                                      1300000000)),
       only_ways(['normal'])
      ],
     compile,
     ['-fcontext-stack=300'])

test('parsing001',
     [# expected value: ?
      if_wordsize(32,
          compiler_stats_num_field('bytes allocated', 280000000,
                                                      320000000)),
      # expected value: 587079016 (amd64/Linux):
      if_wordsize(64,
          compiler_stats_num_field('bytes allocated', 540000000,
                                                      600000000)),
       only_ways(['normal']),
      ],
     compile_fail, [''])


