def no_lint(name, opts):
   opts.compiler_always_flags = \
       filter(lambda opt: opt != '-dcore-lint' and opt != '-dcmm-lint', opts.compiler_always_flags)

setTestOpts(no_lint)


# Note [residency]
#
# Residency (peak_megabytes_allocated and max_bytes_used) is sensitive
# to when the major GC runs, which makes it inherently inaccurate.
# Sometime an innocuous change somewhere can shift things around such
# that the samples occur at a different time, and the residency
# appears to change (up or down) when the underlying profile hasn't
# really changed.
#
# However, please don't just ignore changes in residency.  If you see
# a change in one of these figures, please check whether it is real or
# not as follows:
#
#  * Run the test with old and new compilers, adding +RTS -h -i0.01
#    (you don't need to compile anything for profiling or enable profiling
#    libraries to get a heap profile).
#  * view the heap profiles, read off the maximum residency.  If it has
#    really changed, then you know there's an issue.

test('T1969',
     [compiler_stats_num_field('peak_megabytes_allocated', # Note [residency]
          [(wordsize(32), 17, 15),
             # 2010-05-17 14 (x86/Windows)
             #            15 (x86/OS X)
             #            19 (x86/OS X)
             # 2013-02-10 13 (x86/Windows)
             # 2013-02-10 14 (x86/OSX)
             # 2013-11-13 17 (x86/Windows, 64bit machine)
           (wordsize(64), 30, 15)]),
             #            28 (amd64/Linux)
             #            34 (amd64/Linux)
             # 2012-09-20 23 (amd64/Linux)
             # 2012-10-03 25 (amd64/Linux if .hi exists)
             # 2013-02-13 23, but unstable so increased to 10% range
             # 2013-02-13 27, very unstable!
             # 2013-09-11 30 (amd64/Linux)
      compiler_stats_num_field('max_bytes_used',
          [(platform('i386-unknown-mingw32'), 7295012, 20),
                                 # 2010-05-17 5717704 (x86/Windows)
                                 # 2013-02-10 5159748 (x86/Windows)
                                 # 2013-02-10 5030080 (x86/Windows)
                                 # 2013-11-13 7295012 (x86/Windows, 64bit machine)
           (wordsize(32), 6429864, 1),
             #            6707308 (x86/OS X)
             # 2009-12-31 6149572 (x86/Linux)
             # 2014-01-22 6429864 (x86/Linux)
           (wordsize(64), 11000000, 20)]),
               # looks like the peak is around ~10M, but we're
               # unlikely to GC exactly on the peak.
               # varies quite a lot with CLEANUP and BINDIST,
               # hence 10% range.
               # See Note [residency] to get an accurate view.
      compiler_stats_num_field('bytes allocated',
          [(platform('i386-unknown-mingw32'), 317975916, 5),
                                 #            215582916 (x86/Windows)
                                 # 2012-10-29 298921816 (x86/Windows)
                                 # 2013-02-10 310633884 (x86/Windows)
                                 # 2013-11-13 317975916 (x86/Windows, 64bit machine)
           (wordsize(32), 316103268, 1),
             #            221667908 (x86/OS X)
             #            274932264 (x86/Linux)
             # 2012-10-08 303930948 (x86/Linux, new codegen)
             # 2013-02-10 322937684 (x86/OSX)
             # 2014-01-22 316103268 (x86/Linux)
           (wordsize(64), 660922376, 5)]),
             # 17/11/2009 434845560 (amd64/Linux)
             # 08/12/2009 459776680 (amd64/Linux)
             # 17/05/2010 519377728 (amd64/Linux)
             # 05/08/2011 561382568 (amd64/OS X)
             # 16/07/2012 589168872 (amd64/Linux)
             # 20/07/2012 595936240 (amd64/Linux)
             # 23/08/2012 606230880 (amd64/Linux)
             # 29/08/2012 633334184 (amd64/Linux) new codegen
             # 18/09/2012 641959976 (amd64/Linux)
             # 19/10/2012 661832592 (amd64/Linux) -fPIC turned on
             # 23/10/2012 642594312 (amd64/Linux) -fPIC turned off again
             # 12/11/2012 658786936 (amd64/Linux) UNKNOWN REASON
             # 17/1/13:   667160192 (x86_64/Linux) new demand analyser
             # 18/10/2013 698612512 (x86_64/Linux) fix for #8456
             # 10/02/2014 660922376 (x86_64/Linux) call artiy analysis
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
      compiler_stats_num_field('max_bytes_used', # Note [residency]
          [(wordsize(32), 24009436, 15),
             #            17725476 (x86/OS X)
             #            14593500 (Windows)
             # 2013-02-10 20651576 (x86/Windows)
             # 2013-02-10 20772984 (x86/OSX)
             # 2013-11-13 24009436 (x86/Windows, 64bit machine)
           (wordsize(64), 43224080, 15)]),
             # prev:           25753192 (amd64/Linux)
             # 29/08/2012:     37724352 (amd64/Linux)
             #  (increase due to new codegen, see #7198)
             # 13/13/2012:     44894544 (amd64/Linux)
             #  (reason for increase unknown)
             # 15/5/2013:      36904752  (amd64/Linux)
             #  (reason for decrease unknown)
             # 29/5/2013:      43224080  (amd64/Linux)
             #  (reason for increase back to earlier value unknown)

      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 1565185140, 5),
           # previous:     815479800  (x86/Linux)
           # (^ increase due to new codegen, see #7198)
           # 2012-10-08:  1373514844 (x86/Linux)
           # 2013-11-13: 1478325844  (x86/Windows, 64bit machine)
           # 2014-01-12: 1565185140  (x86/Linux)
           (wordsize(64), 2897630040, 5)]),
            # old:        1357587088 (amd64/Linux)
            # 29/08/2012: 2961778696 (amd64/Linux)
            # (^ increase due to new codegen, see #7198)
            # 18/09/2012: 2717327208 (amd64/Linux)
            # 08/06/2013: 2901451552 (amd64/Linux) (reason unknown)
            # 12/12/2013: 3083825616 (amd64/Linux) (reason unknown)
            # 18/02/2014: 2897630040 (amd64/Linux) (call arity improvements)
      conf_3294
      ],
     compile,
     [''])

test('T4801',
     [ # expect_broken(5224),
       # temporarily unbroken (#5227)
      compiler_stats_num_field('peak_megabytes_allocated',# Note [residency]
          [(platform('x86_64-apple-darwin'), 70, 1),
                           # expected value: 58 (amd64/OS X)
                           # 13/01/2014 - 70
           (wordsize(32), 30, 20),
           (wordsize(64), 60, 20)]),
            # prev:       50 (amd64/Linux)
            # 19/10/2012: 64 (amd64/Linux)
            #                (^ REASON UNKNOWN!)
            # 12/11/2012: 49 (amd64/Linux)
            #                (^ REASON UNKNOWN!)
            # 28/8/13:    60 (amd64/Linux)
            #                (^ REASON UNKNOWN!)

      compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-apple-darwin'), 464872776, 5),
                           # expected value: 510938976 (amd64/OS X):

           (wordsize(32), 211198056, 10),
           # prev:        185669232 (x86/OSX)
           # 2014-01-22:  211198056 (x86/Linux)
           (wordsize(64), 392409984, 10)]),
            # prev:       360243576 (amd64/Linux)
            # 19/10/2012: 447190832 (amd64/Linux) (-fPIC turned on)
            # 19/10/2012: 392409984 (amd64/Linux) (-fPIC turned off)

      compiler_stats_num_field('max_bytes_used',
          [(platform('x86_64-apple-darwin'), 25145320, 5),
           (wordsize(32), 11829000, 15),
             #              9651948 (x86/OSX)
             #              10290952 (windows)
             # 2013-02-10   11071060 (x86/Windows)
             # 2013-02-10:  11207828 (x86/OSX)
             # (some date): 11139444
             # 2013-11-13:  11829000 (x86/Windows, 64bit machine)
           (wordsize(64), 25002136, 10)]),
                # prev:       20486256 (amd64/OS X)
                # 30/08/2012: 17305600--20391920 (varies a lot)
                # 19/10/2012: 26882576 (-fPIC turned on)
                # 19/10/2012: 18619912 (-fPIC turned off)
                # 24/12/2012: 21657520 (perhaps gc sampling time wibbles?)
                # 10/01/2014: 25166280 
                # 13/01/2014: 22646000 (mostly due to #8647)
                # 18/02/2014: 25002136 (call arity analysis changes)
       only_ways(['normal']),
       extra_hc_opts('-static')
      ],
     compile,
     [''])

test('T3064',
     [# expect_broken( 3064 ),
      compiler_stats_num_field('peak_megabytes_allocated',# Note [residency]
          [(wordsize(32), 23, 20),
            # expected value: 14 (x86/Linux 28-06-2012):
            # 2013-11-13:     18 (x86/Windows, 64bit machine)
            # 2014-01-22:     23 (x86/Linux)
           (wordsize(64), 37, 20)]),
            # (amd64/Linux):            18
            # (amd64/Linux) 2012-02-07: 26
            # (amd64/Linux) 2013-02-12: 23; increased range to 10%
            # (amd64/Linux) 2013-04-03: 26
            # (amd64/Linux) 2013-09-11: 30; result of AMP patch
            # Increased range to 20%.  peak-usage varies from 22 to 26,
            #  depending on whether the old .hi file exists
            # (amd64/Linux) 2013-09-11: 37; better arity analysis (weird)

      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 162457940, 10),
            # 2011-06-28: 56380288  (x86/Linux)
            # 2012-10-30: 111189536 (x86/Windows)
            # 2013-11-13: 146626504 (x86/Windows, 64bit machine)
            # 2014-01-22: 162457940 (x86/Linux)
           (wordsize(64), 308422280, 5)]),
            # (amd64/Linux) (28/06/2011):  73259544
            # (amd64/Linux) (07/02/2013): 224798696
            # (amd64/Linux) (02/08/2013): 236404384, increase from roles
            # (amd64/Linux) (11/09/2013): 290165632, increase from AMP warnings
            # (amd64/Linux) (22/11/2013): 308300448, GND via Coercible and counters for constraints solving
	        # (amd64/Linux) (02/12/2013): 329795912, Coercible refactor
            # (amd64/Linux) (11/02/2014): 308422280, optimize Coercions in simpleOptExpr

      compiler_stats_num_field('max_bytes_used',
          [(wordsize(32), 7218200 , 20),
            # 2011-06-28: 2247016 (x86/Linux) (28/6/2011):
            #(some date): 5511604
            # 2013-11-13: 7218200 (x86/Windows, 64bit machine)
           (wordsize(64), 19821544, 20)]),
            # (amd64/Linux, intree) (28/06/2011):  4032024
            # (amd64/Linux, intree) (07/02/2013):  9819288
            # (amd64/Linux)         (14/02/2013):  8687360
            # (amd64/Linux)         (18/02/2013):  9397488
            # (amd64/Linux)         (02/08/2013): 10742536, increase from roles
            # (amd64/Linux)         (19/08/2013): 9211816,  decrease apparently from better eta reduction
            # (amd64/Linux)         (11/09/2013): 12000480, increase from AMP warnings
            #                                     933cdf15a2d85229d3df04b437da31fdfbf4961f
            # (amd64/Linux)         (22/11/2013): 16266992, GND via Coercible and counters for constraints solving
            # (amd64/Linux)         (12/12/2013): 19821544, better One shot analysis
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
          [(wordsize(32), 198573456, 10),
           # previous:    196457520
           # 2012-10-08:  259547660 (x86/Linux, new codegen)
           # 2013-11-21:  198573456 (x86 Windows, 64 bit machine)
           (wordsize(64), 397672152, 10)]),
             # Previously 530000000 (+/- 10%)
             # 17/1/13:   602993184  (x86_64/Linux)
             #            (new demand analyser)
             # 2013-06-08 538467496  (x86_64/Linux)
             # ^ reason unknown
             # 2013-08-02 454498592  (amd64/Linux)
             # decrease from more aggressive coercion optimisations from roles
             # 2013-11-12 397672152  (amd64/Linux)
             # big decrease following better CSE and arity

       only_ways(['normal'])
      ],
     compile,
     ['-fcontext-stack=300'])

test('T5631',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 392904228, 10),
        # expected value: 392904228 (x86/Linux)
           (wordsize(64), 690742040, 5)]),
        # expected value: 774595008 (amd64/Linux):
        # expected value: 735486328 (amd64/Linux) 2012/12/12:
        # expected value: 690742040 (amd64/Linux) Call Arity improvements
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
           (wordsize(64), 654804144, 10)]),
            # prev:       349263216 (amd64/Linux)
            # 07/08/2012: 384479856 (amd64/Linux)
            # 29/08/2012: 436927840 (amd64/Linux)
            # 12/11/2012: 640324528 (amd64/Linux)
            #   (OldCmm removed: not sure why this got worse, the
            #    other perf tests remained about the same)
            # 18/10/2013: 734038080 (amd64/Linux)
            #   (fix for #8456)
            # 24/10/2013: 654804144 (amd64/Linux)
            #   (fix previous fix for #8456)
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
            (wordsize(64), 694019152, 10)])
             # prev:       585521080
             # 29/08/2012: 713385808
             #  (increase due to new codegen)
             # 15/05/2013: 628341952
             #  (reason for decrease unknown)
             # 24/06/2013: 694019152
             #  (reason for re-increase unknown)
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 240302920, 10),
            # prev:       213380256
            # 2012-10-08: 240302920 (x86/Linux)
            #  (increase due to new codegen)
           (wordsize(64), 476497048, 10)])
            # prev:       418306336
            # 29/08/2012: 492905640
            #  (increase due to new codegen)
            # 15/05/2013: 406039584
            #  (reason for decrease unknown)
            # 08/06/2013: 476497048
            #  (reason for increase unknown)
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
          [(wordsize(32), 45520936 , 10),
             # 40000000 (x86/Linux)
             # 2013-11-13: 45520936 (x86/Windows, 64bit machine)
           (wordsize(64), 86795752, 10)])
             # sample: 3926235424 (amd64/Linux, 15/2/2012)
             # 2012-10-02 81879216
             # 2012-09-20 87254264 amd64/Linux
             # 2013-09-18 90587232 amd64/Linux
	     # 2013-11-21 86795752 amd64/Linux, GND via Coercible and counters
	     #                                  for constraints solving
      ],
      compile_fail,['-ftype-function-depth=50'])

test('T6048',
     [ only_ways(['optasm']),
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 48887164, 10),
            # prev:       38000000 (x86/Linux)
            # 2012-10-08: 48887164 (x86/Linux)
           (wordsize(64), 95960720, 10)])
             # 18/09/2012 97247032 amd64/Linux
             # 16/01/2014 108578664 amd64/Linux (unknown, likely foldl-via-foldr)
             # 18/01/2014 95960720 amd64/Linux Call Arity improvements
             # 28/02/2014 105556793 amd64/Linux (unknown, tweak in base/4d9e7c9e3 resulted in change)
      ],
      compile,[''])
