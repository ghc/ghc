# Tests that call 'comparison(_,_,True)' (ie, are compiler performance tests)
# are skipped when debugging is on.  
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
      comparison(['peak_megabytes_allocated','max_bytes_used'],15,True),
      comparison('bytes allocated',5,True),
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
      comparison('max_bytes_used',15,True),
      comparison('bytes allocated',5,True),
      conf_3294,

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      extra_hc_opts('+RTS -G1 -RTS')
      ],
     compile,
     [''])

test('T4801',
     [ # comparison('peak_megabytes_allocated',1,True),
     # expect_broken(5224),
     # temporarily unbroken (#5227)
     # deactivated for now, as this metric became too volatile recently

      comparison('bytes allocated',10,True),

       # comparison('max_bytes_used',5,True),
       # deactivated for now, as this metric became too volatile recently
       only_ways(['normal']),
       extra_hc_opts('-static'),

       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
       ],
     compile,
     [''])

test('T3064',
     [comparison('peak_megabytes_allocated',20,True),

      comparison('bytes allocated',10,True),

      # comparison('max_bytes_used',20,True),
      # deactivated for now, as this metric became too volatile recently

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
     [comparison('bytes allocated',10,True),

       only_ways(['normal'])
      ],
     compile,
     ['-freduction-depth=300'])

test('T5631',
     [comparison('bytes allocated',10,True),
       only_ways(['normal'])
      ],
     compile,
     [''])

test('parsing001',
     [comparison('bytes allocated',10,True),
       only_ways(['normal']),
      ],
     compile_fail, [''])


test('T783',
     [ only_ways(['normal']),  # no optimisation for this one
      # expected value: 175,569,928 (x86/Linux)
      comparison('bytes allocated',10,True),

      extra_hc_opts('-static')
      ],
      compile,[''])

test('T5321Fun',
     [ only_ways(['normal']),  # no optimisation for this one
       comparison('bytes allocated',10,True),
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      comparison('bytes allocated',10,True),
      ],
      compile,[''])

test('T5642',
     [ only_ways(['normal']),
       normal,
       comparison('bytes allocated',10,True),
      ],
      compile,['-O'])

test('T5837',
     [ only_ways(['normal']),
      comparison('bytes allocated',10,True),
      ],
      compile, ['-freduction-depth=50'])

test('T6048',
     [ only_ways(['optasm']),
     comparison('bytes allocated',10,True),
      ],
      compile,[''])

test('T9020',
     [ only_ways(['optasm']),
      comparison('bytes allocated',10,True),
      ],
      compile,[''])

test('T9675',
     [ only_ways(['optasm']),
       comparison('all',15,True),

       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
      ],
     compile,
     [''])

test('T9872a',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
      ],
     compile_fail,
     [''])

test('T9872b',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
      ],
     compile_fail,
     [''])
test('T9872c',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
      ],
     compile_fail,
     [''])
test('T9872d',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
      ],
     compile,
     [''])

test('T9961',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
      ],
     compile,
     ['-O'])

test('T9233',
    [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
      extra_clean(['T9233a.hi', 'T9233a.o'])
    ],
    multimod_compile,
    ['T9233', '-v0 -O2 -fno-spec-constr'])

test('T10370',
     [ only_ways(['optasm']),
       comparison(['max_bytes_used','peak_megabytes_allocated'],15,True),
       # Use `+RTS -G1` for more stable residency measurements. Note [residency].
       extra_hc_opts('+RTS -G1 -RTS')
     ],
     compile,
     [''])

test('T10547',
     [ comparison('bytes allocated',20,True),
     ],
     compile_fail,
     ['-fprint-expanded-synonyms'])

test('T12227',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
     ],
     compile,
     # Use `-M1G` to prevent memory thrashing with ghc-8.0.1.
     ['-O2 -ddump-hi -ddump-to-file +RTS -M1G'])

test('T12425',
     [ only_ways(['optasm']),
       comparison('bytes allocated',5,True),
     ],
     compile,
     [''])

test('T12234',
     [ only_ways(['optasm']),
       comparison('bytes allocated',5,True),
     ],
     compile,
     [''])

test('T12545',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
       extra_clean(['T12545a.hi', 'T12545a.o'])
     ],
     multimod_compile,
     ['T12545', '-v0'] )

test('T13035',
     [ only_ways(['normal']),
       comparison('bytes allocated',5,True),
     ],
     compile,
     [''] )

test('T13056',
     [ only_ways(['optasm']),
       comparison('bytes allocated',10,True),
     ],
     compile,
     ['-O1'])

test('T12707',
     [ comparison('bytes allocated',5,True),
     ],
     compile,
     [''])

test('T12150',
     [ only_ways(['optasm']),
       compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 74358208, 5)
          # initial:    70773000
          # 2017-08-25: 74358208  Refactor the Mighty Simplifier
          ]),
     ],
    compile,
     [''])

test('T13379',
     [ comparison('bytes allocated',10,True),
     ],
     compile,
     [''])

test('MultiLayerModules',
     [ comparison('bytes allocated',10,True),
       pre_cmd('./genMultiLayerModules'),
       extra_files(['genMultiLayerModules']),
     ],
     multimod_compile,
     ['MultiLayerModules', '-v0'])

test('T13701',
     [ comparison('bytes allocated',10,True),
       pre_cmd('./genT13701'),
       extra_files(['genT13701']),
     ],
     multimod_compile,
     ['T13701', '-v0'])

test('T13719',
     [ comparison('bytes allocated',10,True),
       pre_cmd('./genT13719'),
       extra_files(['genT13719']),
     ],
     multimod_compile,
     ['T13719', '-v0'])

test('Naperian',
     [ reqlib('vector'),
       only_ways(['optasm']),
       comparison('bytes allocated',10,True),
     ],
     compile,
     [''])

test ('T9630',
      [ comparison('bytes allocated',15,True),
      extra_clean(['T9630a.hi', 'T9630a.o'])
      ],
      multimod_compile,
      ['T9630', '-v0 -O'])
