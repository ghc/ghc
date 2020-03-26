# Tests that call 'collect_compiler_stats' are skipped when debugging is on.
# See testsuite/driver/testlib.py.

def no_lint(name, opts):
   opts.compiler_always_flags = \
       [opt for opt in opts.compiler_always_flags if opt != '-dcore-lint' and opt != '-dcmm-lint']

setTestOpts(no_lint)

test('T1969',
     [# expect_broken(12437),
      collect_compiler_residency(20),
      extra_run_opts('+RTS -A64k -RTS'),
          # The default RESIDENCY_OPTS is 256k and we need higher sampling
          # frequency. Incurs a slow-down by about 2.
      collect_compiler_stats('bytes allocated', 1),
      only_ways(['normal']),

      extra_hc_opts('-dcore-lint -static'),
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
     [collect_compiler_residency(15),
      collect_compiler_stats('bytes allocated', 1),
      conf_3294,
      ],
     compile,
     [''])

test('T4801',
     [collect_compiler_stats('bytes allocated',2),
      only_ways(['normal']),
      extra_hc_opts('-static'),
      ],
     compile,
     [''])

test('T3064',
     [collect_compiler_residency(20),
      collect_compiler_stats('bytes allocated',2),
      only_ways(['normal']),
      ],
     compile,
     [''])

test('T4007', normal, makefile_test, ['T4007'])

test('T5030',
     [collect_compiler_stats('bytes allocated', 2),

       only_ways(['normal'])
      ],
     compile,
     ['-freduction-depth=300'])

test('T5631',
     [collect_compiler_stats('bytes allocated',2),
      only_ways(['normal'])
      ],
     compile,
     [''])

test('parsing001',
     [collect_compiler_stats('bytes allocated',2),
       only_ways(['normal']),
      ],
     compile_fail, [''])


test('T783',
     [ only_ways(['normal']),  # no optimisation for this one
      collect_compiler_stats('bytes allocated',2),
      extra_hc_opts('-static')
      ],
      compile,[''])

test('T5321Fun',
     [ only_ways(['normal']),  # no optimisation for this one
       collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T5642',
     [ only_ways(['normal']),
       normal,
       collect_compiler_stats('bytes allocated',2)
      ],
      compile,['-O'])

test('T5837',
     [ only_ways(['normal']),
      collect_compiler_stats('bytes allocated',2)
      ],
      compile, ['-freduction-depth=50'])

test('T6048',
     [ only_ways(['optasm']),
      collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T9020',
     [ only_ways(['optasm']),
      collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T9675',
     [ only_ways(['optasm']),
       collect_compiler_residency(15),
       collect_compiler_stats('bytes allocated',2),
      ],
     compile,
     [''])

test('T9872a',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile_fail,
     [''])

test('T9872b',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile_fail,
     [''])
test('T9872c',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile_fail,
     [''])
test('T9872d',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile,
     [''])

test('T9961',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile,
     ['-O'])

test('T9233',
    [ only_ways(['normal']),
      collect_compiler_stats('bytes allocated', 1),
      extra_clean(['T9233a.hi', 'T9233a.o'])
    ],
    multimod_compile,
    ['T9233', '-v0 -O2 -fno-spec-constr'])

test('T10370',
     [ only_ways(['optasm']),
       collect_compiler_residency(15),
     ],
     compile,
     [''])

test('T10547',
     [ collect_compiler_stats('bytes allocated', 4),
     ],
     compile_fail,
     ['-fprint-expanded-synonyms'])

test('T12227',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
     ],
     compile,
     # Use `-M1G` to prevent memory thrashing with ghc-8.0.1.
     ['-O2 -ddump-hi -ddump-to-file +RTS -M1G'])

test('T12425',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated', 1)
     ],
     compile,
     [''])

test('T12234',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated', 1),
     ],
     compile,
     [''])

test('T12545',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
       extra_clean(['T12545a.hi', 'T12545a.o'])
     ],
     multimod_compile,
     ['T12545', '-v0'] )

test('T13035',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
     ],
     compile,
     [''] )

test('T13056',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated',2),
     ],
     compile,
     ['-O1'])

test('T12707',
     [ collect_compiler_stats('bytes allocated', 1),
     ],
     compile,
     [''])

test('T12150',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated', 1)
     ],
    compile,
     [''])

test('T13379',
     [ collect_compiler_stats('bytes allocated',2),
     ],
     compile,
     [''])

test('MultiLayerModules',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genMultiLayerModules'),
       extra_files(['genMultiLayerModules']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     ['MultiLayerModules', '-v0'])

test('ManyConstructors',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genManyConstructors'),
       extra_files(['genManyConstructors']),
     ],
     multimod_compile,
     ['ManyConstructors', '-v0'])

test('ManyAlternatives',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genManyAlternatives'),
       extra_files(['genManyAlternatives']),
     ],
     multimod_compile,
     ['ManyAlternatives', '-v0'])

test('T13701',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genT13701'),
       extra_files(['genT13701']),
       compile_timeout_multiplier(4)
       # 4 is _a lot_ (timeout after 1200s),
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     ['T13701', '-v0'])

test('T13719',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genT13719'),
       extra_files(['genT13719']),
       compile_timeout_multiplier(2)
       # 2 is _a lot_ (timeout after 600s, to build 600 modules),
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     ['T13719', '-v0'])

test('T14697',
     [ collect_compiler_stats('bytes allocated',2),
       # This generates too large of a command-line for poor Windows and
       # Darwin. The solution is to teach the testsuite driver to
       # invoke GHC with a response file.
       when(opsys('mingw32') or opsys('darwin'), fragile(15072)),
       pre_cmd('./genT14697'),
       extra_files(['genT14697']),
       extra_hc_opts('$(cat T14697-flags)'), # 10k -optP arguments
     ],
     multimod_compile,
     ['T14697', '-v0'])

test('T14683',
     [ collect_compiler_stats('bytes allocated',2),
     ],
     multimod_compile,
     ['T14683', '-v0'])

test('Naperian',
     [ reqlib('vector'),
       only_ways(['optasm']),
       collect_compiler_stats('bytes allocated',2),
     ],
     compile,
     [''])

test ('T9630',
      [ collect_compiler_residency(15),
        extra_clean(['T9630a.hi', 'T9630a.o']),
      ],
      multimod_compile,
      ['T9630', '-v0 -O'])

test ('T15164',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])

# See https://gitlab.haskell.org/ghc/ghc/merge_requests/312#note_186960
test ('WWRec',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])

test('T16190',
      [req_th, collect_compiler_stats()],
      multimod_compile,
      ['T16190.hs', '-v0'])

test('T16473', normal, makefile_test, ['T16473'])
