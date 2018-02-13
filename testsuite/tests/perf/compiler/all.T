# Tests that call 'compiler_stats_num_field' are skipped when debugging is on.
# See testsuite/driver/testlib.py.

def no_lint(name, opts):
   opts.compiler_always_flags = \
       [opt for opt in opts.compiler_always_flags if opt != '-dcore-lint' and opt != '-dcmm-lint']

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
     [# expect_broken(12437),
      compiler_stats_num_field('peak_megabytes_allocated', # Note [residency]
          [(wordsize(32), 30, 15),
             # 2010-05-17 14 (x86/Windows)
             #            15 (x86/OS X)
             #            19 (x86/OS X)
             # 2013-02-10 13 (x86/Windows)
             # 2013-02-10 14 (x86/OSX)
             # 2013-11-13 17 (x86/Windows, 64bit machine)
             # 2015-07-11 21 (x86/Linux, 64bit machine) use +RTS -G1
             # 2016-04-06 30 (x86/Linux, 64bit machine)
           (wordsize(64), 78, 20)]),
             #            28 (amd64/Linux)
             #            34 (amd64/Linux)
             # 2012-09-20 23 (amd64/Linux)
             # 2012-10-03 25 (amd64/Linux if .hi exists)
             # 2013-02-13 23, but unstable so increased to 10% range
             # 2013-02-13 27, very unstable!
             # 2014-09-10 29 (amd64/Linux) post-AMP-cleanup
             # 2013-09-11 30, 10 (amd64/Linux)
             # 2013-09-11 30, 15 (adapt to Phab CI)
             # 2015-06-03 41, (amd64/Linux) use +RTS -G1
             # 2015-10-28 55, (amd64/Linux) emit Typeable at definition site
             # 2016-10-20 68, (amd64/Linux) allow top-level string literals
             #                See the comment 16 on #8472.
             # 2017-02-17 83  (amd64/Linux) Type-indexed Typeable
             # 2017-03-31 61  (amd64/Linux) Fix memory leak in simplifier
             # 2018-01-25 78  (amd64/Linux) Use CoreExpr for EvTerm
      compiler_stats_num_field('max_bytes_used',
          [(platform('i386-unknown-mingw32'), 5719436, 20),
                                 # 2010-05-17 5717704 (x86/Windows)
                                 # 2013-02-10 5159748 (x86/Windows)
                                 # 2013-02-10 5030080 (x86/Windows)
                                 # 2013-11-13 7295012 (x86/Windows, 64bit machine)
                                 # 2014-04-24 5719436 (x86/Windows, 64bit machine)
           (wordsize(32), 9418680, 1),
             #            6707308 (x86/OS X)
             # 2009-12-31 6149572 (x86/Linux)
             # 2014-01-22 6429864 (x86/Linux)
             # 2014-06-29 5949188 (x86/Linux)
             # 2015-07-11 6241108 (x86/Linux, 64-bit machine) use +RTS -G1
             # 2016-04-06 9093608 (x86/Linux, 64-bit machine)
             # 2017-03-24 9261052 (x86/Linux, 64-bit machine)
             # 2017-04-06 9418680 (x86/Linux, 64-bit machine)

           (wordsize(64), 19199872, 15)]),
             # 2014-09-10 10463640, 10  # post-AMP-update (somewhat stabelish)
               # looks like the peak is around ~10M, but we're
               # unlikely to GC exactly on the peak.
               # varies quite a lot with CLEANUP and BINDIST,
               # hence 10% range.
               # See Note [residency] to get an accurate view.
             # 2014-09-14  9684256, 10 # try to lower it a bit more to match Phab's CI
             # 2014-11-03 10584344,    # ghcspeed reports higher numbers consistently
             # 2015-07-11 11670120 (amd64/Linux)
             # 2015-10-28 15017528 (amd64/Linux) emit typeable at definition site
             # 2016-10-12 17285216 (amd64/Linux) it's not entirely clear why
             # 2017-02-01 19924328 (amd64/Linux) Join points (#12988)
             # 2017-02-14 16393848 Early inline patch
             # 2017-03-31 16679176 Fix memory leak in simplifier
             # 2017-08-25 19199872 Refactor the Mighty Simplifier

      compiler_stats_num_field('bytes allocated',
          [(platform('i386-unknown-mingw32'), 301784492, 5),
                                 #            215582916 (x86/Windows)
                                 # 2012-10-29 298921816 (x86/Windows)
                                 # 2013-02-10 310633884 (x86/Windows)
                                 # 2013-11-13 317975916 (x86/Windows, 64bit machine)
                                 # 2014-04-04 301784492 (x86/Windows, 64bit machine)
           (wordsize(32), 324586096, 1),
             #            221667908 (x86/OS X)
             #            274932264 (x86/Linux)
             # 2012-10-08 303930948 (x86/Linux, new codegen)
             # 2013-02-10 322937684 (x86/OSX)
             # 2014-01-22 316103268 (x86/Linux)
             # 2014-06-29 303300692 (x86/Linux)
             # 2015-07-11 288699104 (x86/Linux, 64-bit machine) use +RTS -G1
             # 2016-04-06 344730660 (x86/Linux, 64-bit machine)
             # 2017-03-24 324586096 (x86/Linux, 64-bit machine)
           (wordsize(64), 659863176, 5)]),
             # 2009-11-17 434845560 (amd64/Linux)
             # 2009-12-08 459776680 (amd64/Linux)
             # 2010-05-17 519377728 (amd64/Linux)
             # 2011-08-05 561382568 (amd64/OS X)
             # 2012-07-16 589168872 (amd64/Linux)
             # 2012-07-20 595936240 (amd64/Linux)
             # 2012-08-23 606230880 (amd64/Linux)
             # 2012-08-29 633334184 (amd64/Linux) new codegen
             # 2012-09-18 641959976 (amd64/Linux)
             # 2012-10-19 661832592 (amd64/Linux) -fPIC turned on
             # 2012-10-23 642594312 (amd64/Linux) -fPIC turned off again
             # 2012-11-12 658786936 (amd64/Linux) UNKNOWN REASON
             # 2013-91-17 667160192 (x86_64/Linux) new demand analyser
             # 2013-10-18 698612512 (x86_64/Linux) fix for #8456
             # 2014-02-10 660922376 (x86_64/Linux) call arity analysis
             # 2014-07-17 651626680 (x86_64/Linux) roundabout update
             # 2014-09-10 630299456 (x86_64/Linux) post-AMP-cleanup
             # 2015-06-03 581460896 (x86_64/Linux) use +RTS -G1
             # 2015-10-28 695430728 (x86_64/Linux) emit Typeable at definition site
             # 2015-10-28 756138176 (x86_64/Linux) inst-decl defaults go via typechecker (#12220)
             # 2017-02-17 831733376 (x86_64/Linux) Type-indexed Typeable
             # 2017-02-25 695354904 (x86_64/Linux) Early inlining patch
             # 2017-04-21 659863176 (x86_64/Linux) Unknown
      only_ways(['normal']),

      extra_hc_opts('-dcore-lint -static'),
          # Leave -dcore-lint on for this one test, so that we have something
          # that will catch a regression in -dcore-lint performance.

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      extra_hc_opts('+RTS -G1 -RTS')
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
          [(wordsize(32), 28686588, 15),
             #            17725476 (x86/OS X)
             #            14593500 (Windows)
             # 2013-02-10 20651576 (x86/Windows)
             # 2013-02-10 20772984 (x86/OSX)
             # 2013-11-13 24009436 (x86/Windows, 64bit machine)
             # 2014-04-24 19882188 (x86/Windows, 64bit machine)
             # 2014-12-22 26525384 (x86/Windows) Increase due to silent superclasses?
             # 2015-07-11 43196344 (x86/Linux, 64-bit machine) use +RTS -G1
             # 2016-04-06 28686588 (x86/Linux, 64-bit machine)

           (wordsize(64), 34050960, 20)]),
             # prev:           25753192 (amd64/Linux)
             # 29/08/2012:     37724352 (amd64/Linux)
             #  (increase due to new codegen, see #7198)
             # 13/13/2012:     44894544 (amd64/Linux)
             #  (reason for increase unknown)
             # 15/5/2013:      36904752  (amd64/Linux)
             #  (reason for decrease unknown)
             # 29/5/2013:      43224080  (amd64/Linux)
             #  (reason for increase back to earlier value unknown)
             # 2014-07-14:     36670800  (amd64/Linux)
             #  (reason unknown, setting expected value somewhere in between)
             # 2015-01-22:     45000000  (amd64/Linux)
             #  varies between 40959592 and 52914488... increasing to +-20%
             # 2015-10-28:     50367248  (amd64/Linux)
             #  D757: emit Typeable instances at site of type definition
             # 2016-07-11:     54609256  (Windows) before fix for #12227
             # 2016-07-11:     52992688  (Windows) after fix for #12227
             # 2017-02-17:     63131248  (amd64/Linux) Type indexed Typeable
             # 2017-05-14:     34050960  (amd64/Linux) Two-pass CmmLayoutStack

      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 1377050640, 5),
           # previous:     815479800  (x86/Linux)
           # (^ increase due to new codegen, see #7198)
           # 2012-10-08:  1373514844 (x86/Linux)
           # 2013-11-13: 1478325844  (x86/Windows, 64bit machine)
           # 2014-01-12: 1565185140  (x86/Linux)
           # 2013-04-04: 1377050640  (x86/Windows, 64bit machine)
           (wordsize(64), 1858491504, 5)]),
            # old:        1357587088 (amd64/Linux)
            # 29/08/2012: 2961778696 (amd64/Linux)
            # (^ increase due to new codegen, see #7198)
            # 18/09/2012: 2717327208 (amd64/Linux)
            # 08/06/2013: 2901451552 (amd64/Linux) (reason unknown)
            # 12/12/2013: 3083825616 (amd64/Linux) (reason unknown)
            # 18/02/2014: 2897630040 (amd64/Linux) (call arity improvements)
            # 12/03/2014: 2705289664 (amd64/Linux) (more call arity improvements)
            # 2014-17-07: 2671595512 (amd64/Linux) (round-about update)
            # 2014-09-10: 2709595808 (amd64/Linux) post-AMP cleanup
            # 2016-07-11: 2664479936 (Windows) before fix for #12227
            # 2016-07-11: 2739731144 (Windows) after fix for #12227 (ignoring)
            # 2017-02-17: 2758641264 (amd64/Linux) (Type indexed Typeable)
            # 2017-05-14: 2253557280 (amd64/Linux) Two-pass CmmLayoutStack
            # 2017-10-24: 1858491504 (amd64/Linux) Improved linear regAlloc
      conf_3294,

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      extra_hc_opts('+RTS -G1 -RTS')
      ],
     compile,
     [''])

test('T4801',
     [ # expect_broken(5224),
       # temporarily unbroken (#5227)
###################################
# deactivated for now, as this metric became too volatile recently
#      compiler_stats_num_field('peak_megabytes_allocated',# Note [residency]
#          [(platform('x86_64-apple-darwin'), 70, 1),
#                           # expected value: 58 (amd64/OS X)
#                           # 13/01/2014 - 70
#           (wordsize(32), 30, 20),
#           (wordsize(64), 48, 20)]),
#            # prev:       50 (amd64/Linux)
#            # 19/10/2012: 64 (amd64/Linux)
#            #                (^ REASON UNKNOWN!)
#            # 12/11/2012: 49 (amd64/Linux)
#            #                (^ REASON UNKNOWN!)
#            # 28/8/13:    60 (amd64/Linux)
#            #                (^ REASON UNKNOWN!)
#            # 2014-09-10: 55 post-AMP-cleanup
#            # 2014-10-08: 62 (jumps between 55 and 71 observed -- GC tipping point?)
#            # 2014-10-13: 48 stricter seqDmdType

      compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-apple-darwin'), 417302064, 10),
           # prev:       510938976 (amd64/OS X):
           # 2015-12-11: 465653312 (amd64/OS X) Update, bump tolerance to +/-10%
           # 2017-03-24: 417302064 (amd64/OS X) Correlated with Linux improvement

           (wordsize(32), 199856388, 10),
           # prev:        185669232 (x86/OSX)
           # 2014-01-22:  211198056 (x86/Linux)
           # 2014-09-03:  185242032 (Windows laptop)
           # 2014-12-01:  203962148 (Windows laptop)
           # 2016-04-06:  239556572 (x86/Linux)
           # 2017-03-24:  199856388 (x86/Linux)
           (wordsize(64), 388898280, 10)]),
            # prev:       360243576 (amd64/Linux)
            # 19/10/2012: 447190832 (amd64/Linux) (-fPIC turned on)
            # 19/10/2012: 392409984 (amd64/Linux) (-fPIC turned off)
            # 2014-04-08: 362939272 (amd64/Linux) cumulation of various smaller improvements over recent commits
            # 2014-10-08: 382056344 (amd64/Linux) stricter foldr2 488e95b
            # 2015-10-28: 434278248 (amd64/Linux) emit Typeable at definition site
            # 2016-10-19: 388898280 (amd64/Linux) Refactor traceRn interface (#12617)

###################################
# deactivated for now, as this metric became too volatile recently
#
#     compiler_stats_num_field('max_bytes_used',
#         [(platform('x86_64-apple-darwin'), 25145320, 5),
#          (wordsize(32), 11829000, 15),
#            #              9651948 (x86/OSX)
#            #              10290952 (windows)
#            # 2013-02-10   11071060 (x86/Windows)
#            # 2013-02-10:  11207828 (x86/OSX)
#            # (some date): 11139444
#            # 2013-11-13:  11829000 (x86/Windows, 64bit machine)
#          (wordsize(64), 19296544, 15)]),
#               # prev:       20486256 (amd64/OS X)
#               # 30/08/2012: 17305600--20391920 (varies a lot)
#               # 19/10/2012: 26882576 (-fPIC turned on)
#               # 19/10/2012: 18619912 (-fPIC turned off)
#               # 24/12/2012: 21657520 (perhaps gc sampling time wibbles?)
#               # 10/01/2014: 25166280
#               # 13/01/2014: 22646000 (mostly due to #8647)
#               # 18/02/2014: 25002136 (call arity analysis changes)
#               # 12/05/2014: 25002136 (specialisation and inlining changes)
#               # 10/09/2014: 19296544, 10 (post-AMP-cleanup)
#               # 14/09/2014: 19585456, 15 (adapt to Phab CI env)
       only_ways(['normal']),
       extra_hc_opts('-static'),

       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
       ],
     compile,
     [''])

test('T3064',
     [compiler_stats_num_field('peak_megabytes_allocated',# Note [residency]
          [(wordsize(32), 36, 20),
            # expected value: 14 (x86/Linux 28-06-2012):
            # 2013-11-13:     18 (x86/Windows, 64bit machine)
            # 2014-01-22:     23 (x86/Linux)
            # 2014-12-22:     23 (x86/Linux) death to silent superclasses
            # 2015-07-11:     28 (x86/Linux, 64-bit machine) use +RTS -G1
            # 2017-04-06:     36 (x86/Linux, 64-bit machine) it's unclear

           (wordsize(64), 66, 20)]),
            # (amd64/Linux):            18
            # (amd64/Linux) 2012-02-07: 26
            # (amd64/Linux) 2013-02-12: 23; increased range to 10%
            # (amd64/Linux) 2013-04-03: 26
            # (amd64/Linux) 2013-09-11: 30; result of AMP patch
            # Increased range to 20%.  peak-usage varies from 22 to 26,
            #  depending on whether the old .hi file exists
            # (amd64/Linux) 2013-09-11: 37; better arity analysis (weird)
            # (amd64/Linux) (09/09/2014): 42, AMP changes (larger interfaces, more loading)
            # (amd64/Linux) 2014-10-13: 38: Stricter seqDmdType
            # (amd64/Linux) 2014-12-22: 27: death to silent superclasses
            # (amd64/Linux) 2015-01-22: 32: Varies from 30 to 34, at least here.
            # (amd64/Linux) 2015-06-03: 54: use +RTS -G1
            # (amd64/Linux) 2016-10-25: 66: Presumably creep

      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 134044092, 10),
            # 2011-06-28: 56380288  (x86/Linux)
            # 2012-10-30: 111189536 (x86/Windows)
            # 2013-11-13: 146626504 (x86/Windows, 64bit machine)
            # 2014-01-22: 162457940 (x86/Linux)
            # 2014-12-01: 162457940 (Windows)
            # 2014-12-22: 122836340 (Windows) Death to silent superclasses
            # 2016-04-06: 153261024 (x86/Linux) probably wildcard refactor
            # 2017-03-24: 134044092 (x86/Linux, 64-bit machine) Update

           (wordsize(64), 258505536, 5)]),
            # (amd64/Linux) (2011-06-28):  73259544
            # (amd64/Linux) (2013-02-07): 224798696
            # (amd64/Linux) (2013-08-02): 236404384, increase from roles
            # (amd64/Linux) (2013-09-11): 290165632, increase from AMP warnings
            # (amd64/Linux) (2013-11-22): 308300448, GND via Coercible and counters for constraints solving
            # (amd64/Linux) (2013-12-02): 329795912, Coercible refactor
            # (amd64/Linux) (2014-02-11): 308422280, optimize Coercions in simpleOptExpr
            # (amd64/Linux) (2014-05-23): 324022680, unknown cause
            # (amd64/Linux) (2014-07-17): 332702112, general round of updates
            # (amd64/Linux) (2014-08-29): 313638592, w/w for INLINABLE things
            # (amd64/Linux) (2014-09-09): 407416464, AMP changes (larger interfaces, more loading)
            # (amd64/Linux) (2014-09-14): 385145080, BPP changes (more NoImplicitPrelude in base)
            # (amd64/Linux) (2014-12-10): 363103840, improvements in constraint solver
            # (Mac)         (2014-12-18): 350418600, improvements to flattener
            # (amd64/Linux) (2014-12-22): 243670824, Ha! Death to superclass constraints, makes
            #                                        much less code for Monad instances
            # (amd64/Linux) (2015-12-01): 264952256, Regression due to Simon's wildcard refactor
            #                                        Tracked as #11151.
            # (amd64/Linux) (2015-12-11): 304344936, Regression due to TypeInType
            #                                        Tracked as #11196
            # (amd64/Linux) (2016-04-15): 287460128  Improvement due to using coercionKind instead
            #                                        of zonkTcType (Trac #11882)
            # (amd64/Darwin) (2017-01-23): 306222424 Presumably creep from recent changes (Typeable?)
            # (amd64/Linux) (2017-02-14): 259815560  Early inline patch: 9% improvement
            # (amd64/Linux) (2017-03-31): 265950920  Fix memory leak in simplifier
            # (amd64/Linux) (2017-05-01): 281509496  Avoid excessive space usage from unfoldings in CoreTidy
            # (amd64/Linux) (2017-05-01): 258505536  I think this is improvement in coercionKind e4ab65bd

###################################
# deactivated for now, as this metric became too volatile recently
#
#      compiler_stats_num_field('max_bytes_used',
#          [(wordsize(32), 11202304, 20),
#            # 2011-06-28:  2247016 (x86/Linux) (28/6/2011):
#            #(some date):  5511604
#            # 2013-11-13:  7218200 (x86/Windows, 64bit machine)
#            # 2014-04-04: 11202304 (x86/Windows, 64bit machine)
#           (wordsize(64), 13251728, 20)]),
#            # (amd64/Linux, intree) (28/06/2011):  4032024
#            # (amd64/Linux, intree) (07/02/2013):  9819288
#            # (amd64/Linux)         (14/02/2013):  8687360
#            # (amd64/Linux)         (18/02/2013):  9397488
#            # (amd64/Linux)         (02/08/2013): 10742536, increase from roles
#            # (amd64/Linux)         (19/08/2013): 9211816,  decrease apparently from better eta reduction
#            # (amd64/Linux)         (11/09/2013): 12000480, increase from AMP warnings
#            #                                     933cdf15a2d85229d3df04b437da31fdfbf4961f
#            # (amd64/Linux)         (22/11/2013): 16266992, GND via Coercible and counters for constraints solving
#            # (amd64/Linux)         (12/12/2013): 19821544, better One shot analysis
#            # (amd64/Linux)         (09/09/2014): 24357392, AMP changes (larger interfaces, more loading)
#            # (amd64/Linux)         (14/09/2014): 16053888, BPP changes (more NoImplicitPrelude in base)
#            # (amd64/Linux)         (19/09/2014): 18744992, unknown
#            # (amd64/Linux)         2014-10-13:   13251728, Stricter seqDmdType

       only_ways(['normal']),

       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
      ],
     compile,
     [''])

test('T4007',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4007'])

test('T5030',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 345668088, 10),
           # previous:    196457520
           # 2012-10-08:  259547660 (x86/Linux, new codegen)
           # 2013-11-21:  198573456 (x86 Windows, 64 bit machine)
           # 2014-12-10:  227205560 constraint solver got worse again; more aggressive solving
           #                        of family-applications leads to less sharing, I think
           # 2015-07-11:  201882912 reason unknown
           # 2016-04-06:  345668088 likely TypeInType

           (wordsize(64), 794426536, 10)]),
             # Previously 530000000 (+/- 10%)
             # 17/1/13:   602993184  (x86_64/Linux)
             #            (new demand analyser)
             # 2013-06-08 538467496  (x86_64/Linux)
             # ^ reason unknown
             # 2013-08-02 454498592  (amd64/Linux)
             # decrease from more aggressive coercion optimisations from roles
             # 2013-11-12 397672152  (amd64/Linux)
             # big decrease following better CSE and arity
             # 2014-07-17 409314320  (amd64/Linux)
             # general round of updates
             # 2014-09-10 385152728  post-AMP-cleanup
             # 2014-12-08 340969128  constraint solver perf improvements (esp kick-out)
             # 2014-12-10 449042120  constraint solver got worse again; more aggressive solving
             #                          of family-applications leads to less sharing, I think
             # 2015-03-17 403932600  tweak to solver algorithm
             # 2015-12-11 653710960  TypeInType (see #11196)
             # 2016-10-17 794426536  20% big increase following
             #                       31621b12 * A collection of type-inference refactorings.
             #                       See ticket for more info

       only_ways(['normal'])
      ],
     compile,
     ['-freduction-depth=300'])

test('T5631',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 570137436, 10),
        # expected value: 392904228 (x86/Linux)
        # 2014-04-04:     346389856 (x86 Windows, 64 bit machine)
        # 2014-12-01:     390199244 (Windows laptop)
        # 2016-04-06:     570137436 (amd64/Linux) many reasons
           (wordsize(64), 1106015512, 5)]),
        # expected value: 774595008 (amd64/Linux):
        # expected value: 735486328 (amd64/Linux) 2012/12/12:
        # expected value: 690742040 (amd64/Linux) Call Arity improvements
        # 2014-09-09:     739704712 (amd64/Linux) AMP changes
        # 2014-11-04:     776121120 (amd64/Linux) new-flatten-skolems
        # 2015-06-01:     812288344 (amd64/Linux) unknown cause
        # 2015-12-11:     1128828928 (amd64/Linux) TypeInType (see #11196)
        # 2015-12-21:     1198327544 (Mac) TypeApplications (will fix with #11196)
        # 2015-03-18:     1124068664 (Mac) optimize Unify & zonking
        # 2016-10-19:     1024926024 (amd64/Linux) Refactor traceRn interface (#12617)
        # 2016-11-10:     1077429456 (amd64/Linux) Stop -dno-debug-output suppressing -ddump-tc-trace
        # 2017-02-17:     1517484488 (amd64/Linux) Type-indexed Typeable
        # 2017-03-03:     1065147968 (amd64/Linux) Share Typeable KindReps
        # 2017-03-31:     1037482512 (amd64/Linux) Fix memory leak in simplifier
	# 2017-07-27:     1106015512 (Mac) Regresssion from tracking visibility in TypeEqOrigin
	#                                  should be fixed by #14037
       only_ways(['normal'])
      ],
     compile,
     [''])

test('parsing001',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 232777056, 10),
        # Initial:        274000576
        # 2017-03-24:     232777056
           (wordsize(64), 490228304, 5)]),
        # expected value: 587079016 (amd64/Linux)
        # 2016-09-01:     581551384 (amd64/Linux) Restore w/w limit (#11565)
        # 2016-12-19:     493730288 (amd64/Linux) Join points (#12988)
        # 2017-02-14:     463931280 Early inlining patch; acutal improvement 7%
        # 2017-12-11:     490228304 BlockArguments
       only_ways(['normal']),
      ],
     compile_fail, [''])


test('T783',
     [ only_ways(['normal']),  # no optimisation for this one
      # expected value: 175,569,928 (x86/Linux)
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 225911912, 5),
            # 2012-10-08: 226907420 (x86/Linux)
            # 2013-02-10: 329202116 (x86/Windows)
            # 2013-02-10: 338465200 (x86/OSX)
            # 2014-04-04: 319179104 (x86 Windows, 64 bit machine)
            # 2014-09-03: 223377364 (Windows) better specialisation, raft of core-to-core optimisations
            # 2014-12-22: 235002220 (Windows) not sure why
            # 2016-04-06: 249332816 (x86/Linux, 64-bit machine)
            # 2017-03-24: 225911912 (x86/Linux, 64-bit machine)

           (wordsize(64), 481875416, 10)]),
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
            # 2014-07-17: 640031840 (amd64/Linux)
            #   (general round of updates)
            # 2014-08-29: 441932632 (amd64/Linux)
            #   (better specialisation, raft of core-to-core optimisations)
            # 2014-08-29: 719814352 (amd64/Linux)
            #   (changed order of cmm block causes analyses to allocate much more,
            #      but the changed order is slighly better in terms of runtime, and
            #      this test seems to be an extreme outlier.)
            # 2015-05-16: 548288760 (amd64/Linux)
            #   (improved sequenceBlocks in nativeCodeGen, #10422)
            # 2015-08-07: 470738808 (amd64/Linux)
            #   (simplifying the switch plan code path for simple checks, #10677)
            # 2015-08-28: 526230456 (amd64/Linux)
            #    (D757: Emit Typeable instances at site of type definition)
            # 2015-12-04: 1134085384 (amd64/Linux)
            #    (D1535: Major overhaul of pattern match checker, #11162)
            # 2016-02-03: 488592288 (amd64/Linux)
            #    (D1795: Another overhaul of pattern match checker, #11374)
            # 2017-02-14    436978192    Early inlining: 5% improvement
            # 2017-09-08    481875416    Unknown

      extra_hc_opts('-static')
      ],
      compile,[''])

test('T5321Fun',
     [ only_ways(['normal']),  # no optimisation for this one
       compiler_stats_num_field('bytes allocated',
           [(wordsize(32), 244387620, 10),
             # prev:       300000000
             # 2012-10-08: 344416344 x86/Linux
             #  (increase due to new codegen)
             # 2014-09-03: 299656164     (specialisation and inlining)
             # 2014-12-10: 206406188     #  Improvements in constraint solver
             # 2016-04-06: 279922360 x86/Linux
             # 2017-03-24: 244387620 x86/Linux (64-bit machine)

            (wordsize(64), 423774560, 5)])
             # prev:       585521080
             # 2012-08-29: 713385808     #  (increase due to new codegen)
             # 2013-05-15: 628341952     #  (reason for decrease unknown)
             # 2013-06-24: 694019152     #  (reason for re-increase unknown)
             # 2014-05-12: 614409344     #  (specialisation and inlining changes)
             # 2014-09-10: 601629032     #  post-AMP-cleanup
             # 2014-11-06: 541287000     #  Simon's flat-skol changes to the constraint solver
             # 2014-12-10: 408110888     #  Improvements in constraint solver
             # 2014-12-16: 429921312     #  Flattener parameterized over roles
             # 2015-08-10: 509921312
             #  (undefined now takes an implicit parameter and GHC -O0 does
             #  not recognize that the application is bottom)
             # 2015-12-11: 565883176     #  TypeInType (see #11196)
             # 2017-01-06: 497356688     #  Small coercion optimisations
                                         #  The actual decrease was only 2%; earlier
                                         #    commits had drifted down
             # 2017-01-22: 525895608     #  Allow top-level string literals in Core. I'm not
                                         #    convinced that this patch is
                                         #    responsible for all of this
                                         #    change, however. Namely I am
                                         #    quite skeptical of the downward
                                         #    "drift" reported above
             # 2017-01-31: 498135752     #  Join points (#12988)
             # 2017-02-23: 524706256     #  Type-indexed Typeable? (on Darwin)
             # 2017-02-25: 488295304     #  Early inlining patch
             # 2017-05-14: 449577856     #  (amd64/Linxu) Two-pass CmmLayoutStack
             # 2017-12-13: 423774560     #  (amd64/Linxu) Typechecker improvements
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 250757460, 10),
            # prev:       213380256
            # 2012-10-08: 240302920 (x86/Linux)
            #  (increase due to new codegen)
            # 2014-07-31: 211699816 (Windows) (-11%)
            #  (due to better optCoercion, 5e7406d9, #9233)
            # 2016-04-06: 250757460 (x86/Linux)

           (wordsize(64), 415136648, 10)])
            # prev:       418306336
            # 29/08/2012: 492905640
            #  (increase due to new codegen)
            # 15/05/2013: 406039584
            #  (reason for decrease unknown)
            # 08/06/2013: 476497048
            #  (reason for increase unknown)
            # before 2014-07-17: 441997096
            #  (with -8%, still in range, hence cause not known)
            # 2014-07-17: 426960992 (-11% of previous value)
            #  (due to better optCoercion, 5e7406d9, #9233)
            # 2014-10-08  410895536
            #  (various changes; biggest improvements due to 949ad67 and FastString package ids)
            # 2015-08-10: 470895536
            #  (undefined now takes an implicit parameter and GHC -O0 does
            #  not recognize that the application is bottom)
            # 2015-10-28: 532365376
            #  D757: emit Typeable instances at site of type definition
            # 2016-07-16: 477840432
            #  Optimize handling of built-in OccNames
            # 2017-05-14: 415136648 (amd64/Linux) Two-pass CmmLayoutStack
      ],
      compile,[''])

test('T5642',
     [ only_ways(['normal']),
       normal,
       compiler_stats_num_field('bytes allocated',
           [(wordsize(32), 413517560, 10),
                     # sample from x86/Linux
            # prev:        650000000
            # 2014-09-03:  753045568
            # 2014-12-10:  641085256 Improvements in constraints solver
            # 2016-04-06:  462677300
            # 2017-03-24:  413517560 (x86/Linux, 64-bit machine)

            (wordsize(64),  838316496, 10)])
            # prev:        1300000000
            # 2014-07-17:  1358833928 (general round of updates)
            # 2014-08-07:  1402242360 (caused by 1fc60ea)
# Watch out for:
            # 23/05/2014:  1452688392 (More aggressive specialisation means we get
            #                          specialised copies of imported functions that
            #                          are ultimately discarded by trimAutoRules
            #                          It's a bizarre program with LOTS of data types)
            # 2014-09-10:  1536924976 post-AMP-cleanup
            # 2014-12-10:  1282916024 Improvements in constraints solver
            # 2015-10-28:  1412808976 Emit Typeable at definition site
            # 2015-11-22:  1071915072 Use TypeLits in the metadata encoding
            # 2016-02-08:   950004816 Pattern match checker re-rework
            # 2016-05-12:  1300685592 Make Generic1 poly-kinded
            # 2016-06-05:   916484672 Refactor derived Generic instances to reduce allocations
            # 2016-09-03:   838316496 Derive the Generic instance in perf/compiler/T5642
      ],
      compile,['-O'])

test('T5837',
     [ only_ways(['normal']),
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 27028956, 10),
             # 40000000 (x86/Linux)
             # 2013-11-13:  45520936 (x86/Windows, 64bit machine)
             # 2014-09-03:  37096484 (Windows laptop, w/w for INLINABLE things
             # 2014-12-01: 135914136 (Windows laptop, regression see below)
             # 2014-12-08: 115905208  Constraint solver perf improvements (esp kick-out)
             # 2016-04-06: 24199320  (x86/Linux, 64-bit machine) TypeInType
             # 2017-03-24: 27028956  (x86/Linux, 64-bit machine)

           (platform('x86_64-unknown-mingw32'), 54793816, 7),
             # 2017-02-19                       59161648 (x64/Windows) - Unknown
             # 2017-04-21                       54985248 (x64/Windows) - Unknown
             # 2017-12-24                       54793816 (x64/Windows) - Unknown

           (wordsize(64), 52089424, 7)])
             # sample: 3926235424 (amd64/Linux, 15/2/2012)
             # 2012-10-02 81879216
             # 2012-09-20 87254264 amd64/Linux
             # 2013-09-18 90587232 amd64/Linux
             # 2013-11-21 86795752 amd64/Linux, GND via Coercible and counters
             #                                  for constraints solving
             # 2014-08-29 73639840 amd64/Linux, w/w for INLINABLE things
             # 2014-10-08 73639840 amd64/Linux, Burning Bridges and other small changes
             # 2014-11-06 271028976       Linux, Accept big regression;
             #   See Note [An alternative story for the inert substitution] in TcFlatten
             # 2014-12-08 234790312 Constraint solver perf improvements (esp kick-out)
             # 2014-12-16 231155640 Mac  Flattener parameterized over roles;
             #                           some optimization
             # 2015-03-17 53424304  Mac  Better depth checking; fails earlier
             # 2015-06-09 38834096  Better "improvement"; I'm not sure whey it improves things
             # 2015-12-11 43877520  amd64/Linux, TypeInType (see #11196)
             # 2016-03-18 48507272  Mac, accept small regression in exchange
             #                           for other optimisations
             # 2016-09-15 42445672  Linux; fixing #12422
             # 2016-09-25 41832056  amd64/Linux, Rework handling of names (D2469)
             # 2016-10-25 52597024  amd64/Linux, the test now passes (hooray), and so
             #                          allocates more because it goes right down the
             #                          compilation pipeline
             # 2017-01-24 57861352  amd64/Linux, very likely due to the top-level strings
             #                          in Core patch.
             # 2017-02-07 50253880  Another improvement in SetLevels.  I don't think
             #                      all the gain here is from this patch, but I think it
             #                      just pushed it over the edge, so I'm re-centreing, and
             #                      changing to 5% tolerance
             # 2017-02-07 53592736  amd64/Linux Simon's earlier decrease appears
             #                      to be environmentally-dependent.
             #                      Also bumped acceptance threshold to 7%.
             # 2017-02-20 58648600  amd64/Linux Type-indexed Typeable
             # 2017-02-28 54151864  amd64/Linux Likely drift due to recent simplifier improvements
             # 2017-02-25 52625920  amd64/Linux Early inlining patch
             # 2017-09-06 56782344  amd64/Linux Drift manifest in unrelated LLVM patch
             # 2017-10-24 52089424  amd64/linux Fix space leak in BinIface.getSymbolTable
      ],
      compile, ['-freduction-depth=50'])

test('T6048',
     [ only_ways(['optasm']),
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 55701280, 10),
            # prev:       38000000 (x86/Linux)
            # 2012-10-08: 48887164 (x86/Linux)
            # 2014-04-04: 62618072 (x86 Windows, 64 bit machine)
            # 2014-09-03: 56315812 (x86 Windows, w/w for INLINABLE)
            # 2014-12-01: 49987836 (x86 Windows)
            # 2016-04-06: 55701280 (x86/Linux, 64-bit machine)

           (wordsize(64), 90996312, 10)])
             # 2012-09-18  97247032 amd64/Linux
             # 2014-01-16 108578664 amd64/Linux (unknown, likely foldl-via-foldr)
             # 2014-01-18  95960720 amd64/Linux Call Arity improvements
             # 2014-02-28 105556793 amd64/Linux (unknown, tweak in base/4d9e7c9e3 resulted in change)
             # 2014-03-05 110646312 amd64/Linux Call Arity became more elaborate
             # 2014-07-14 125431448 amd64/Linux unknown reason. Even worse in GHC-7.8.3. *shurg*
             # 2014-08-29 108354472 amd64/Linux w/w for INLINABLE things
             # 2014-09-14  88186056 amd64/Linux BPP part1 change (more NoImplicitPreludes in base)
             # 2014-01-08  95946688 amd64/Linux Mostly 4c834fd. Occasional spikes to 103822120!
             # 2016-03-11 108225624 amd64/Linux unknown reason sadly; likely gradual creep.
             # 2016-11-25  94327392 amd64/Linux Back down again hooray; still not sure why
             # 2017-02-17 115715592 amd64/Linux Type-indexed Typeable
             # 2017-04-28 90996312 Join point refactoring
      ],
      compile,[''])

test('T9020',
     [ only_ways(['optasm']),
      compiler_stats_num_field('bytes allocated',
          [(wordsize(32), 249904136, 10),
           # Original:    381360728
           # 2014-07-31:  343005716 (Windows) (general round of updates)
           # 2017-03-24:  249904136 (x86/Linux, 64-bit machine)

           (wordsize(64), 423163832, 10)])
           # prev:        795469104
           # 2014-07-17:  728263536 (general round of updates)
           # 2014-09-10:  785871680 post-AMP-cleanup
           # 2014-11-03:  680162056 Further Applicative and Monad adjustments
           # 2015-10-21:  786189008 Make stronglyConnCompFromEdgedVertices deterministic
           # 2016-01-26:  698401736 improvement from using ExpTypes instead of ReturnTvs
           # 2016-04-06:  852298336 Refactoring of CSE #11781
           # 2016-04-06:  698401736 Use thenIO in Applicative IO
           # 2017-02-03:  764866144 Join points
           # 2017-02-14:  500707080 Early inline patch; 35% decrease!
           #                        Program size collapses in first simplification
           # 2017-03-31:  493596312 Fix memory leak in simplifier
           # 2017-04-28:  423163832  Remove exponential behaviour in simplifier
      ],
      compile,[''])

test('T9675',
     [ only_ways(['optasm']),
       compiler_stats_num_field('max_bytes_used', # Note [residency]
          [(wordsize(64), 17675240, 15),
          # 2014-10-13    29596552
          # 2014-10-13    26570896   seq the DmdEnv in seqDmdType as well
          # 2014-10-13    18582472   different machines giving different results..
          # 2014-10-13    22220552   use the mean
          # 2015-06-21    28056344   switch to `+RTS -G1`, tighten bound to 15%
          # 2015-10-28    23776640   emit Typeable at definition site
          # 2015-12-11    30837312   TypeInType (see #11196)
          # 2016-03-14    38776008   Final demand analyzer run
          # 2016-04-01    29871032   Fix leaks in demand analysis
          # 2016-04-30    17675240   Fix leaks in tidy unfoldings
           (wordsize(32), 18043224, 15)
          # 2015-07-11    15341228   (x86/Linux, 64-bit machine) use +RTS -G1
          # 2016-04-06    18043224   (x86/Linux, 64-bit machine)
          ]),
       compiler_stats_num_field('peak_megabytes_allocated', # Note [residency]
          [(wordsize(64), 63, 15),
          # 2014-10-13    66
          # 2014-10-13    58         seq the DmdEnv in seqDmdType as well
          # 2014-10-13    49         different machines giving different results...
          # 2014-10-13    53         use the mean
          # 2015-06-15    44         reduced for some reason
          # 2015-06-21    105        switch to `+RTS -G1`
          # 2015-12-04    88         new pattern checker (D1535)
          # 2015-12-11    113        TypeInType (see #11196)
          # 2016-04-14    144        Final demand analyzer run
          # 2016-07-26    121        Unboxed sums?
          # 2017-04-30    63         Fix leaks in tidy unfoldings
            (wordsize(32), 56, 15)
          # 2015-07-11    56         (x86/Linux, 64-bit machine) use +RTS -G1
          ]),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 656137960, 10)
          # 2014-10-13    544489040
          # 2015-10-28    608284152  emit Typeable at definition site
          # 2017-02-17    731171072  Type-indexed Typeable
          # 2017-03-13    656137960  Put join ceiling underneath lambdas?

          ,(wordsize(32), 322901484, 10)
          # 2015-07-11    279480696  (x86/Linux, 64-bit machine) use +RTS -G1
          # 2017-03-24    322901484  (x86/Linux, 64-bit machine)

          ]),

       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
      ],
     compile,
     [''])

test('T9872a',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 3005891848, 5),
          # 2014-12-10    5521332656    Initally created
          # 2014-12-16    5848657456    Flattener parameterized over roles
          # 2014-12-18    2680733672    Reduce type families even more eagerly
          # 2015-12-11    3581500440    TypeInType (see #11196)
          # 2016-04-07    3352882080    CSE improvements
          # 2016-10-19    3134866040    Refactor traceRn interface (#12617)
          # 2017-02-17    3298422648    Type-indexed Typeable
          # 2017-02-25    3005891848    Early inlining patch

           (wordsize(32), 1493198244, 5)
          # was           1325592896
          # 2016-04-06    1740903516    x86/Linux
          # 2017-03-24    1493198244    x86/Linux, 64-bit machine
          ]),
      ],
     compile_fail,
     [''])

test('T9872b',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 3730686224, 5),
          # 2014-12-10    6483306280    Initally created
          # 2014-12-16    6892251912    Flattener parameterized over roles
          # 2014-12-18    3480212048    Reduce type families even more eagerly
          # 2015-12-11    5199926080    TypeInType (see #11196)
          # 2016-02-08    4918990352    Improved a bit by tyConRolesRepresentational
          # 2016-04-06:   4600233488    Refactoring of CSE #11781
          # 2016-09-15:   4069522928    Fix #12422
          # 2017-02-14    3730686224    Early inlining: 5% improvement

           (wordsize(32), 1894037608, 5)
          # was           1700000000
          # 2016-04-06    2422750696    x86/Linux
          # 2017-03-24    1894037608    x86/Linux, 64-bit machine
          ]),
      ],
     compile_fail,
     [''])
test('T9872c',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 3404346032, 5),
          # 2014-12-10    5495850096    Initally created
          # 2014-12-16    5842024784    Flattener parameterized over roles
          # 2014-12-18    2963554096    Reduce type families even more eagerly
          # 2015-12-11    4723613784    TypeInType (see #11196)
          # 2016-02-08    4454071184    Improved a bit by tyConRolesRepresentational
          # 2016-04-06:   4306667256    Refactoring of CSE #11781
          # 2016-09-15:   3702580928    Fixing #12422
          # 2017-02-14    3404346032    Early inlining: 5% improvement

           (wordsize(32), 1727582260, 5)
          # was           1500000000
          # 2016-04-06    2257242896
          # 2017-03-24    1727582260    x86/Linux, 64-bit machine
          ]),
      ],
     compile_fail,
     [''])
test('T9872d',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 462817352, 5),
          # 2014-12-18    796071864   Initally created
          # 2014-12-18    739189056   Reduce type families even more eagerly
          # 2015-01-07    687562440   TrieMap leaf compression
          # 2015-03-17    726679784   tweak to solver; probably flattens more
          # 2015-12-11    566134504   TypeInType; see #11196
          # 2016-02-08    534693648   Improved a bit by tyConRolesRepresentational
          # 2016-03-18    506691240   optimize Unify & zonking
          # 2016-12-05    478169352   using tyConIsTyFamFree, I think, but only
          #                           a 1% improvement 482 -> 478
          # 2017-02-17    535565128   Type-indexed Typeable
          # 2017-02-25    498855104   Early inlining
          # 2017-03-03    462817352   Share Typeable KindReps

           (wordsize(32), 232954000, 5)
          # some date     328810212
          # 2015-07-11    350369584
          # 2016-04-06    264566040   x86/Linux
          # 2017-03-24    232954000   x86/Linux, 64-bit machine
          ]),
      ],
     compile,
     [''])

test('T9961',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 498326216, 5),
          # 2015-01-12    807117816   Initally created
          # 2015-spring   772510192   Got better
          # 2015-05-22    663978160   Fix for #10370 improves it more
          # 2015-10-28    708680480   x86_64/Linux   Emit Typeable at definition site
          # 2015-12-17    745044392   x86_64/Darwin  Creep upwards
          # 2016-03-20    519436672   x64_64/Linux   Don't use build desugaring for large lists (#11707)
          # 2016-03-24    568526784   x64_64/Linux   Add eqInt* variants (#11688)
          # 2016-09-01    537297968   x64_64/Linux   Restore w/w limit (#11565)
          # 2016-12-19    571246936   x64_64/Linux   Join points (#12988)
          # 2017-02-14    498326216   Early inline patch; 13% improvement

           (wordsize(32), 255409052, 5)
          # was           375647160
          # 2016-04-06    275264188   x86/Linux
          # 2017-03-24    255409052   x86/Linux, 64-bit machine
          ]),
      ],
     compile,
     ['-O'])

test('T9233',
    [ only_ways(['normal']),
      compiler_stats_num_field('bytes allocated',
        [(wordsize(64),  924299320, 5),
         # 2015-08-04    999826288     initial value
         # 2016-04-14   1066246248     Final demand analyzer run
         # 2016-06-18    984268712     shuffling around of Data.Functor.Identity
         # 2017-01-20    920101608     Improvement to SetLevels apparently saved 4.2% in
         #                             compiler allocation.  Program size seems virtually
         #                             unchanged; maybe the compiler itself is a little faster
         # 2017-01-23    861862608     worker/wrapper evald-ness flags; another 5% improvement!
         # 2017-02-01    894486272     Join points
         # 2017-02-07    884436192     Another improvement to SetLevels
         # 2017-02-17    974530192     Type-indexed Typeable
         # 2017-03-21    924299320     It's unclear

         (wordsize(32),  460112888, 5)
         # 2016-04-06    515672240     (x86/Linux) initial value
         # 2017-03-24    460112888     x86/Linux, 64-bit machine
        ]),
      extra_clean(['T9233a.hi', 'T9233a.o'])
    ],
    multimod_compile,
    ['T9233', '-v0 -O2 -fno-spec-constr'])

test('T10370',
     [ only_ways(['optasm']),
       compiler_stats_num_field('max_bytes_used', # Note [residency]
          [(wordsize(64), 31524048, 15),
          # 2015-10-22    19548720
          # 2016-02-24    22823976   Changing Levity to RuntimeRep; not sure why this regresses though, even after some analysis
          # 2016-04-14    28256896   final demand analyzer run
          # 2016-08-08    33049304
          #     This change happened because we changed the behavior
          #     of inlining across hs-boot files, so that we don't
          #     inline if something comes from a boot file.  This
          #     affected stats on bootstrapped GHC.  However,
          #     when I set -i0.01 with profiling, the heap profiles
          #     were identical, so I think it's just GC noise.
          # 2016-10-20    38221184   Allow top-level string literals.
          #                          See the comment 16 on #8472.
          # 2017-02-17    51126304   Type-indexed Typeable
          # 2017-02-27    43455848   Likely drift from recent simplifier improvements
          # 2017-02-25    41291976   Early inline patch
          # 2017-04-30    31524048   Fix leaks in tidy unfoldings

           (wordsize(32), 19276304, 15),
          # 2015-10-22    11371496
          # 2017-03-24    19276304 (x86/Linux, 64-bit machine)
          ]),
       compiler_stats_num_field('peak_megabytes_allocated', # Note [residency]
          [(wordsize(64), 117, 15),
          # 2015-10-22     76
          # 2016-04-14    101 final demand analyzer run
          # 2016-08-08    121 see above
          # 2017-01-18    146 Allow top-level string literals in Core
          # 2017-02-17    187 Type-indexed Typeable
          # 2017-02-25    154 Early inline patch
          # 2017-04-30    117 Fix leaks in tidy unfoldings
           (wordsize(32),  69, 15),
          # 2015-10-22     39
          # 2017-03-24     69
          ]),
       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
     ],
     compile,
     [''])

test('T10547',
     [ compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-unknown-mingw32'), 37485128, 20),
          # 2017-02-19                         37485128 (x64/Windows) - Unknown

           (wordsize(64), 37681360, 20),
          # initial:    39165544
          # 2016-11-25: 31041520 Linux   Around the time of refactoring the constraint solver;
          #                              but I think that only pushed it over the edge
          # 2017-02-20: 38681216 Linux   Type-indexed Typeable
          ]),
     ],
     compile_fail,
     ['-fprint-expanded-synonyms'])

test('T12227',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 812869424, 5),
          # 2016-07-11    5650186880 (Windows) before fix for #12227
          # 2016-07-11    1822822016 (Windows) after fix for #12227
          # 2016-12-20    1715827784 after d250d493 (INLINE in Traversable dms)
          #                          (or thereabouts in the commit history)
          # 2017-02-14    1060158624  Early inlining: 35% improvement
          # 2018-01-04    812869424   Drop unused givens (#13032): 23% better
          ]),
     ],
     compile,
     # Use `-M1G` to prevent memory thrashing with ghc-8.0.1.
     ['-O2 -ddump-hi -ddump-to-file +RTS -M1G'])

test('T12425',
     [ only_ways(['optasm']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 134780272, 5),
          # initial:      125831400
          # 2017-01-18:   133380960  Allow top-level string literals in Core
          # 2017-02-17:   153611448  Type-indexed Typeable
          # 2017-03-03:   142256192  Share Typeable KindReps
          # 2017-03-21:   134334800  Unclear
          # 2017-04-28:   127500136  Remove exponential behaviour in simplifier
          # 2017-05-23:   134780272  Addition of llvm-targets in dynflags (D3352)
          ]),
     ],
     compile,
     [''])

test('T12234',
     [ only_ways(['optasm']),
       compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-unknown-mingw32'), 79889200, 5),
          # initial:      83032768
          # 2017-02-19    89180624 (x64/Windows) - Unknown
          # 2017-02-25    79889200 (x64/Windows) - Early inline patch
           (wordsize(64), 81696664, 5),
          # initial:      72958288
          # 2016-01-17:   76848856  (x86-64, Linux. drift?)
          # 2017-02-01:   80882208  (Use superclass instances when solving)
          # 2017-02-05:   74374440  (Probably OccAnal fixes)
          # 2017-02-17:   86525344  (Type-indexed Typeable)
          # 2017-02-25:   83032768  (Early inline patch)
          # 2017-09-07:   81696664  (Semigroup=>Monoid patch, D3927)
          ]),
     ],
     compile,
     [''])

test('T12545',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 3538652464, 5),
          # 2017-06-08    3538652464  initial
          ]),
       extra_clean(['T12545a.hi', 'T12545a.o'])
     ],
     multimod_compile,
     ['T12545', '-v0'] )

test('T13035',
     [ only_ways(['normal']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 118665640, 5),
          # 2017-01-05   90595208  initial
          # 2017-01-19   95269000  Allow top-level string literals in Core
          # 2017-02-05   88806416  Probably OccAnal fixes
          # 2017-02-17   103890200 Type-indexed Typeable
          # 2017-02-25   98390488  Early inline patch
          # 2017-03-21   93249744  It's unclear
          # 2017-07-19   118665640 Generate Typeable bindings for data instances
          ]),
     ],
     compile,
     [''] )

test('T13056',
     [ only_ways(['optasm']),
       compiler_stats_num_field('bytes allocated',
         [(wordsize(64), 440548592, 10),
         # 2017-01-06    520166912 initial
         # 2017-01-31    546800240 Join points (#12988)
         # 2017-02-07    524611224 new SetLevels
         # 2017-02-14    440548592 Early inline patch: 16% improvement
         # 2017-04-21    417860736 (darwin)
         # 2017-04-22    Increase to +/- 10% (Darwin and Linux differ significantly)
         ]),
     ],
     compile,
     ['-O1'])

test('T12707',
     [ compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 1163821528, 5),
          # initial:    1271577192
          # 2017-01-22: 1348865648  Allow top-level strings in Core
          # 2017-01-31: 1280336112  Join points (#12988)
          # 2017-02-11: 1310037632  Check local family instances vs imports
          # 2017-02-23: 1386110512  Type-indexed Typeable? (on Darwin)
          # 2017-03-02: 1231809592  Drift from recent simplifier improvements
          # 2017-05-14: 1163821528  (amd64/Linux) Two-pass CmmLayoutStack
          ]),
     ],
     compile,
     [''])

test('T12150',
     [ only_ways(['optasm']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 73769936, 5)
          # initial:    70773000
          # 2017-08-25: 74358208  Refactor the Mighty Simplifier
          # 2017-08-25: 78300680  Drift
          # 2017-10-25: 73769936  amd64/linux Fix space leak in BinIface.getSymbolTable
          ]),
     ],
    compile,
     [''])

test('T13379',
     [ compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-apple-darwin'), 453166912, 10),
          # 453166912: add osx-specific after two-pass CmmLayoutStack
           (wordsize(64), 411597856, 10),
          # initial:    411597856
          # widen window to 10%, Darwin had 449080520, a 9.1% difference
          ]),
     ],
     compile,
     [''])

test('MultiLayerModules',
     [ compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 5619893176, 10),
          # initial:    12139116496
          # 2017-05-12: 6956533312   Revert "Use a deterministic map for imp_dep_mods"
          # 2017-05-31: 6294813000   Faster checkFamInstConsistency
          # 2018-01-21: 5619893176   Allocate less in plus_mod_dep
          ]),
       pre_cmd('./genMultiLayerModules'),
       extra_files(['genMultiLayerModules']),
     ],
     multimod_compile,
     ['MultiLayerModules', '-v0'])

test('ManyConstructors',
     [ compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 4246959352, 10),
          # initial:    8130527160
          # 2018-01-05: 4246959352  Lift constructor tag allocation out of a loop
          ]),
       pre_cmd('./genManyConstructors'),
       extra_files(['genManyConstructors']),
     ],
     multimod_compile,
     ['ManyConstructors', '-v0'])

test('ManyAlternatives',
     [ compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 1398898072, 10),
          # initial:    1756999240
          # 2018-01-20: 1398898072  Use IntSet in Dataflow
          ]),
       pre_cmd('./genManyAlternatives'),
       extra_files(['genManyAlternatives']),
     ],
     multimod_compile,
     ['ManyAlternatives', '-v0'])

test('T13701',
     [ compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-apple-darwin'), 2217187888, 10),
           (platform('x86_64-unknown-linux'), 2133380768, 10),
           # initial:     2511285600
           # 2017-06-23:  2188045288    treat banged variable bindings as FunBinds
           # 2017-07-11:  2187920960
           # 2017-07-12:  2412223768    inconsistency between Ben's machine and Harbormaster?
           # 2017-07-17:  2133380768    Resolved the issue causing the inconsistencies in this test
          ]),
       pre_cmd('./genT13701'),
       extra_files(['genT13701']),
     ],
     multimod_compile,
     ['T13701', '-v0'])

test('T13719',
     [ compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 5187889872, 10),
          # initial:    49907410784
          # 2017-05-31: 5187889872   Faster checkFamInstConsistency
          ]),
       pre_cmd('./genT13719'),
       extra_files(['genT13719']),
     ],
     multimod_compile,
     ['T13719', '-v0'])

test('Naperian',
     [ reqlib('vector'),
       only_ways(['optasm']),
       compiler_stats_num_field('bytes allocated',
          [(platform('x86_64-unknown-mingw32'), 54116696, 10),
           # 2017-12-24                       54116696 (x64/Windows) - Unknown
          (wordsize(64), 53576760, 10)])
           # 2018-01-25                       53576760 (x64/Linux) - The previous value looked very wrong
     ],
     compile,
     [''])

test ('T9630',
      [ compiler_stats_num_field('max_bytes_used', # Note [residency]
          [(platform('x86_64-unknown-mingw32'),   39867088, 15),
          # 2017-12-24:                     34171816 (x64/Windows)
          (wordsize(64), 41568168, 15)
          # initial:    56955240
          # 2017-06-07: 41568168     Stop the specialiser generating loopy code
          ]),
      extra_clean(['T9630a.hi', 'T9630a.o'])
      ],
      multimod_compile,
      ['T9630', '-v0 -O'])
