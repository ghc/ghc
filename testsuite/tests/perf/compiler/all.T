def no_lint(name, opts):
   opts.compiler_always_flags = \
       filter(lambda opt: opt != '-dcore-lint' and opt != '-dcmm-lint', opts.compiler_always_flags)

setTestOpts(no_lint)


test('T1969',
     [compiler_stats_num_field('peak_megabytes_allocated',
          [(wordsize(32), 14, 1),
             # 2010-05-17 14 (x86/Windows)
             #            15 (x86/OS X)
             #            19 (x86/OS X)
             # 2013-02-10 13 (x86/Windows)
             # 2013-02-10 14 (x86/OSX)
           (wordsize(64), 23, 10)]),
             #            28 (amd64/Linux)
             #            34 (amd64/Linux)
             # 2012-09-20 23 (amd64/Linux)
             # 2012-10-03 25 (amd64/Linux if .hi exists)
             # 2013-02-13 23, but unstable so increased to 10% range
      compiler_stats_num_field('max_bytes_used',
          [(platform('i386-unknown-mingw32'), 5094914, 2),
                                 # 2010-05-17 5717704 (x86/Windows)
                                 # 2013-02-10 5159748 (x86/Windows)
                                 # 2013-02-10 5030080 (x86/Windows)
           (wordsize(32), 6149572, 1),
             #            6707308 (x86/OS X)
             # 2009-12-31 6149572 (x86/Linux)
           (wordsize(64), 9000000, 20)]),
               # looks like the peak is around 10M, but we're
               # unlikely to GC exactly on the peak.
               # varies quite a lot with CLEANUP and BINDIST,
               # hence 10% range.
      compiler_stats_num_field('bytes allocated',
          [(platform('i386-unknown-mingw32'), 310633884, 1),
                                 #            215582916 (x86/Windows)
                                 # 2012-10-29 298921816 (x86/Windows)
                                 # 2013-02-10 310633884 (x86/Windows)
           (wordsize(32), 322937684, 1),
             #            221667908 (x86/OS X)
             #            274932264 (x86/Linux)
             # 2012-10-08 303930948 (x86/Linux, new codegen)
             # 2013-02-10 322937684 (x86/OSX)
           (wordsize(64), 658786936, 5)]),
             # 17/11/2009 434845560 (amd64/Linux)
             # 08/12/2009 459776680 (amd64/Linux)
             # 17/05/2010 519377728 (amd64/Linux)
             # 05/08/2011 561382568 (amd64/OS X)
             # 16/07/2012 589168872 (amd64/Linux)
             # 20/07/2012 595936240 (amd64/Linux)
             # 23/08/2012 606230880 (amd64/Linux)
             # 29/08/2012 633334184 (amd64/Linux)
             #            (^ new codegen)
             # 18/09/2012 641959976 (amd64/Linux)
             # 19/10/2012 661832592 (amd64/Linux)
             #            (^ -fPIC turned on)
             # 23/10/2012 642594312 (amd64/Linux)
             #            (^ -fPIC turned off again)
             # 12/11/2012 658786936 (amd64/Linux)
             #            (^ UNKNOWN REASON )
             # 17/1/13:   667160192 (x86_64/Linux)
             #            (^ new demand analyser)
      only_ways(['normal']),

      extra_hc_opts('-dcore-lint -static')
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
     [
      compiler_stats_num_field('max_bytes_used',
          [(wordsize(32), 20712280, 1),
             #            17725476 (x86/OS X)
             #            14593500 (Windows)
             # 2013-02-10 20651576 (x86/Windows)
             # 2013-02-10 20772984 (x86/OSX)
           (wordsize(64), 44894544, 15)]),
        # prev:           25753192 (amd64/Linux)
        # 29/08/2012:     37724352 (amd64/Linux)
        #  (increase due to new codegen, see #7198)
        # 13/13/2012:     44894544 (amd64/Linux)
        #  (reason for increase unknown)
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 1373514844, 5),
           # previous:     815479800  (x86/Linux)
           # (^ increase due to new codegen, see #7198)
           # 2012-10-08:  1373514844 (x86/Linux)
           (wordsize(64), 2717327208, 5)]),
            # old:        1357587088 (amd64/Linux)
            # 29/08/2012: 2961778696 (amd64/Linux)
            # (^ increase due to new codegen, see #7198)
            # 18/09/2012: 2717327208 (amd64/Linux)
      conf_3294
      ],
     compile,
     [''])

test('T4801',
     [ # expect_broken(5224),
       # temporarily unbroken (#5227)
      compiler_stats_num_field('peak_megabytes_allocated',
          [(platform('x86_64-apple-darwin'), 58, 1),
                           # expected value: 58 (amd64/OS X)
           (wordsize(32), 30, 20),
           (wordsize(64), 49, 20)]),
            # prev:       50 (amd64/Linux)
            # 19/10/2012: 64 (amd64/Linux)
            #                (^ REASON UNKNOWN!)
            # 12/11/2012: 49 (amd64/Linux)
            #                (^ REASON UNKNOWN!)

      compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-apple-darwin'), 510938976, 5),
                           # expected value: 510938976 (amd64/OS X):

           (wordsize(32), 185669232, 10),
        # expected value: 228286660 (x86/OS X)

           (wordsize(64), 392409984, 10)]),
            # prev:       360243576 (amd64/Linux)
            # 19/10/2012: 447190832 (amd64/Linux) (-fPIC turned on)
            # 19/10/2012: 392409984 (amd64/Linux) (-fPIC turned off)

      compiler_stats_num_field('max_bytes_used',
          [(platform('x86_64-apple-darwin'), 21657520, 5),
           (wordsize(32), 11139444, 1),
             #             9651948 (x86/OSX)
             #            10290952 (windows)
             # 2013-02-10 11071060 (x86/Windows)
             # 2013-02-10 11207828 (x86/OSX)
           (wordsize(64), 21657520, 15)]),
                # prev:       20486256 (amd64/OS X)
                # 30/08/2012: 17305600--20391920 (varies a lot)
                # 19/10/2012: 26882576 (-fPIC turned on)
                # 19/10/2012: 18619912 (-fPIC turned off)
                # 24/12/2012: 21657520 (perhaps gc sampling time wibbles?)
       only_ways(['normal']),
       extra_hc_opts('-static')
      ],
     compile,
     [''])

test('T3064',
     [# expect_broken( 3064 ),
      compiler_stats_num_field('peak_megabytes_allocated',
          [(wordsize(32), 14, 1),
        # expected value: 14 (x86/Linux 28-06-2012):
           (wordsize(64), 23, 10)]),
            # (amd64/Linux):            18
            # (amd64/Linux) 2012-02-07: 26
            # (amd64/Linux) 2013-02-12: 23; increased range to 10%

      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 111189536, 10),
         # expected value: 56380288 (x86/Linux) (28/6/2011)
         #                111189536 (x86/Windows) (30/10/12)
           (wordsize(64), 224798696, 5)]),
            # (amd64/Linux) (28/06/2011):  73259544
            # (amd64/Linux) (07/02/2013): 224798696

      compiler_stats_num_field('max_bytes_used',
          [(wordsize(32), 5511604, 20),
        # expected value: 2247016 (x86/Linux) (28/6/2011):
           (wordsize(64), 9819288, 5)]),
            # (amd64/Linux, intree) (28/06/2011):  4032024
            # (amd64/Linux, intree) (07/02/2013):  9819288
       only_ways(['normal'])
      ],
     compile,
     [''])

test('T4007',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4007'])

test('T5030',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 259547660, 10),
           # previous:    196457520
           # 2012-10-08:  259547660 (x86/Linux, new codegen)
           (wordsize(64), 602993184, 10)]),
             # Previously 530000000 (+/- 10%)
             # 17/1/13:   602993184  (x86_64/Linux)
             #            (new demand analyser)
   
       only_ways(['normal'])
      ],
     compile,
     ['-fcontext-stack=300'])

test('T5631',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 392904228, 10),
        # expected value: 392904228 (x86/Linux)
           (wordsize(64), 774595008, 5)]),
        # expected value: 774595008 (amd64/Linux):
       only_ways(['normal'])
      ],
     compile,
     [''])

test('parsing001',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 274000576, 10),
           (wordsize(64), 587079016, 5)]),
        # expected value: 587079016 (amd64/Linux)
       only_ways(['normal']),
      ],
     compile_fail, [''])


test('T783',
     [ only_ways(['normal']),  # no optimisation for this one
      # expected value: 175,569,928 (x86/Linux)
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 333833658, 2),
            # 2012-10-08: 226907420 (x86/Linux)
            # 2013-02-10: 329202116 (x86/Windows)
            # 2013-02-10: 338465200 (x86/OSX)
           (wordsize(64), 640324528, 10)]),
            # prev:       349263216 (amd64/Linux)
            # 07/08/2012: 384479856 (amd64/Linux)
            # 29/08/2012: 436927840 (amd64/Linux)
            # 12/11/2012: 640324528 (amd64/Linux)
            #   (OldCmm removed: not sure why this got worse, the
            #    other perf tests remained about the same)
      extra_hc_opts('-static')
      ],
      compile,[''])

test('T5321Fun',
     [ only_ways(['normal']),  # no optimisation for this one
       compiler_stats_num_field('bytes allocated',
           [(wordsize(32), 344416344, 10),
             # prev:       300000000
             # 2012-10-08: 344416344 x86/Linux
             #  (increase due to new codegen)
            (wordsize(64), 713385808, 10)])
             # prev:       585521080
             # 29/08/2012: 713385808
             #  (increase due to new codegen)
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 240302920, 10),
            # prev:       213380256
            # 2012-10-08: 240302920 (x86/Linux)
            #  (increase due to new codegen)
           (wordsize(64), 492905640, 10)])
            # prev:       418306336
            # 29/08/2012: 492905640
            #  (increase due to new codegen)
      ],
      compile,[''])

test('T5642',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
           [(wordsize(32), 650000000, 10),
                     # sample from x86/Linux
            (wordsize(64), 1300000000, 10)])
      ],
      compile,['-O'])

test('T5837',
     [ only_ways(['normal']),
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 40000000, 10),
          # sample from x86/Linux
           (wordsize(64), 81879216, 10)])
              # sample: 3926235424 (amd64/Linux, 15/2/2012)
             # 2012-10-02 81879216
             # 2012-09-20 87254264 amd64/Linux
      ],
      compile_fail,['-fcontext-stack=50'])

test('T6048',
     [ only_ways(['optasm']),
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 48887164, 10),
            # prev:       38000000 (x86/Linux)
            # 2012-10-08: 48887164 (x86/Linux)
           (wordsize(64), 97247032, 10)])
             # 18/09/2012 97247032 amd64/Linux
      ],
      compile,[''])
