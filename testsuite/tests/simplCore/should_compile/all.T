test('simpl001', normal, compile, [''])
test('simpl002', normal, compile, [''])
test('simpl003', normal, compile, [''])
test('simpl004', normal, compile, [''])
test('simpl005', normal, compile, [''])

# Missing:
# test('simpl006', normal, compile, [''])

test('simpl007', normal, compile, [''])
# Missing: simpl008
test('simpl009', normal, compile, [''])
test('simpl010', normal, compile, [''])
test('simpl011', normal, compile, [''])
test('simpl012', normal, compile, [''])
test('simpl013', normal, compile, [''])
test('simpl014', normal, compile, [''])
test('simpl016', normal, compile, ['-dsuppress-uniques'])
test('simpl017', normal, compile_fail, [''])
test('simpl018', normal, compile, [''])
test('simpl019', normal, compile, [''])
test('simpl020', [extra_files(['Simpl020_A.hs'])], multimod_compile, ['simpl020', '-v0'])

test('simpl-T1370', normal, compile, [''])
test('T2520', normal, compile, [''])

test('spec001', when(fast(), skip), compile, [''])
test('spec002', normal, compile, [''])
test('spec003', normal, compile, [''])

test('rule1', normal, compile, [''])
test('rule2', only_ways(['optasm']), compile, ['-dsuppress-uniques'])
test('T7162', normal, compile, [''])

test('dfun-loop', normal, compile, [''])
test('strict-float', normal, compile, [''])
test('T3118', normal, compile, [''])
test('T4203', normal, compile, [''])

# With -prof -fvia-C, this test makes gcc go out to lunch, and the
# testsuite driver times out.
test('T3016', when(fast(), skip), compile, [''])

test('T1647', normal, compile, ['-fdicts-strict -dcore-lint'])

test('T3234',
     only_ways(['optasm']),
     compile,
     ['-ddump-simpl-stats -dsuppress-uniques'])

test('T3717', only_ways(['optasm']),
              compile,
              ['-ddump-simpl -dsuppress-uniques'])

test('spec-inline', only_ways(['optasm']),
              	    compile,
              	    ['-O2 -ddump-simpl -dsuppress-uniques'])
test('T4908', only_ways(['optasm']),
              compile,
              ['-O2 -ddump-simpl -dsuppress-uniques'])

test('T4930', only_ways(['optasm']),
              compile,
              ['-O -ddump-simpl -dsuppress-uniques'])

test('T3055', [], run_command, ['$MAKE -s --no-print-directory T3055'])

test('T4306',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4306'])

test('T4201',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4201'])

test('T3772',
#      only_ways(['optasm']),
     extra_clean(['T3772_A.hi', 'T3772_A.o']),
     run_command, ['$MAKE -s --no-print-directory T3772'])

test('EvalTest',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory EvalTest'])

test('T3831', normal, compile, [''])
test('T4345', normal, compile, [''])
test('T4398', normal, compile, ['-dsuppress-uniques'])

test('T4903', [], run_command, ['$MAKE -s --no-print-directory T4903'])

test('T4918', [], run_command, ['$MAKE -s --no-print-directory T4918'])

# This test flips too and fro about whether it passes
# I'm not sure why it is so delicate, but it greps the
# result of -ddump-simpl, which is never advertised to
# be very stable
test('T4945',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4945'])

test('T4957',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4957'])

test('T5168',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5168'])

test('T5329', normal, compile, [''])
test('T5303', normal, compile, [''])   # Coercion-optimiation test
test('T5342', normal, compile, [''])   # Lint error with -prof
test('T5359a', normal, compile, [''])  # Lint error with -O (OccurAnal)
test('T5359b', normal, compile, [''])  # Lint error with -O (OccurAnal)
test('T5458', normal, compile, [''])
test('simpl021', [extra_files(['Simpl021A.hs', 'Simpl021B.hs'])], run_command, ['$MAKE -s --no-print-directory simpl021'])
test('T5327', normal, run_command, ['$MAKE -s --no-print-directory T5327'])
test('T5615', normal, run_command, ['$MAKE -s --no-print-directory T5615'])
test('T5623', normal, run_command, ['$MAKE -s --no-print-directory T5623'])
test('T13155', normal, run_command, ['$MAKE -s --no-print-directory T13155'])
test('T5658b',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5658b'])
test('T5776',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5776'])
test('T4138', [], run_command, ['$MAKE -s --no-print-directory T4138'])
test('T7088', normal, compile, [''])
test('T6082-RULE', normal, compile, [''])
test('T7165', [], run_command, ['$MAKE -s --no-print-directory T7165'])
test('T7287', normal, compile, [''])
test('T7360', only_ways(['optasm']), compile, ['-ddump-simpl -dsuppress-uniques'])
test('T5366',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5366'])
test('T7796', [], run_command, ['$MAKE -s --no-print-directory T7796'])
test('T5550', omit_ways(prof_ways), compile, [''])
test('T7865', normal, run_command, ['$MAKE -s --no-print-directory T7865'])
test('T7785', only_ways(['optasm']), compile, ['-ddump-rules'])
test('T7702',
     [extra_files(['T7702plugin']),
      pre_cmd('$MAKE -s --no-print-directory -C T7702plugin package.T7702 TOP={top}'),
      clean_cmd('$MAKE -s --no-print-directory -C T7702plugin clean.T7702'),
      # we say 18mb peak allocated +/- 70% because other compiler flags have
      # a large effect on allocation which is hard to separate from the
      # allocation done by the plugin... but a regression allocates > 90mb
      compiler_stats_num_field('peak_megabytes_allocated',
          [(wordsize(32), 18, 70), (wordsize(64), 18, 70)])
     ],
     compile,
     ['-v0 -package-db T7702plugin/pkg.T7702/local.package.conf -fplugin T7702Plugin -package T7702plugin ' + config.plugin_way_flags])

test('T7944', normal, compile, ['-O2'])
test('T7995',
     # RULE doesn't seem to fire unless optimizations are turned on.
     # This seems reasonable, so I've required it for the test. -- EZY 20130720
     only_ways(['optasm']),
     run_command,
     ['$MAKE -s --no-print-directory T7995'])

test('T8196', normal, compile, [''])
test('T8221b', normal, compile, [''])
test('T8221', [], run_command, ['$MAKE -s --no-print-directory T8221'])
test('T8274', normal, run_command, ['$MAKE -s --no-print-directory T8274'])
test('T8329', only_ways(['optasm']), multimod_compile, ['T8329','-v0 -O'])
test('T5996',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5996'])
test('T8537', normal, compile, [''])
test('T8832',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T8832 T8832_WORDSIZE_OPTS=' +
      ('-DT8832_WORDSIZE_64' if wordsize(64) else '')])
test('T8848', normal,
     run_command,
     ['$MAKE -s --no-print-directory T8848'])
test('T8848a', only_ways(['optasm']), compile, ['-ddump-rules'])
test('T8331', only_ways(['optasm']), compile, ['-ddump-rules'])
test('T6056', only_ways(['optasm']), multimod_compile, ['T6056', '-v0 -ddump-rule-firings'])
test('T9400', only_ways(['optasm']), compile, ['-O0 -ddump-simpl -dsuppress-uniques'])
test('T9583', only_ways(['optasm']), compile, [''])
test('T9565', only_ways(['optasm']), compile, [''])
test('T5821', only_ways(['optasm']), compile, [''])
test('T10176', only_ways(['optasm']), compile, [''])
test('T10180', only_ways(['optasm']), compile, [''])
test('T10602', only_ways(['optasm']), multimod_compile, ['T10602','-v0'])
test('T10627', only_ways(['optasm']), compile, [''])
test('T10181', [only_ways(['optasm'])], compile, [''])
test('T10083',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T10083'])
test('T10689', normal, compile, [''])
test('T11155',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T11155'])
test('T11232', normal, compile, ['-O2'])
test('T11562', normal, compile, ['-O2'])
test('T11742', normal, compile, ['-O2'])
test('T11644', normal, compile, ['-O2'])
test('T3990',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T3990'])

test('T12076', [], multimod_compile, ['T12076', '-v0'])
test('T12076lit', normal, compile, ['-O'])
test('T12076sat', normal, compile, ['-O'])
test('T12212', normal, compile, ['-O'])

test('noinline01', only_ways(['optasm']), compile, ['-ddump-stg -dsuppress-uniques -O'])
test('par01', only_ways(['optasm']), compile, ['-ddump-prep -dsuppress-uniques -O2'])
test('T12776', normal, compile, ['-O2'])
test('T9509',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T9509'])
test('T12603',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T12603'])
test('T12877', normal, run_command, ['$MAKE -s --no-print-directory T12877'])
test('T13027', normal, compile, [''])
test('T13025',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T13025'])
test('T13143', only_ways(['optasm']), compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T13156', normal, run_command, ['$MAKE -s --no-print-directory T13156'])
test('T11444', normal, compile, [''])
test('str-rules',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory str-rules'])
test('T13170', only_ways(['optasm']), compile, ['-dcore-lint'])
test('T13317',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T13317'])
test('T13340', normal, run_command, ['$MAKE -s --no-print-directory T13340'])
test('T13338', only_ways(['optasm']), compile, ['-dcore-lint'])
test('T13367', normal, run_command, ['$MAKE -s --no-print-directory T13367'])
test('T13417', normal, compile, ['-O'])
test('T13413', normal, compile, [''])
test('T13410', normal, compile, ['-O2'])
test('T13468',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T13468'])
test('T13543', normal, compile, ['-ddump-str-signatures'])
test('T11272',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T11272'])
test('T12600',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T12600'])
test('T13658',
     [when((arch('powerpc64') or arch('powerpc64le')), expect_broken(11261))],
     compile, ['-dcore-lint'])
test('T13708', normal, compile, [''])

# thunk should inline here, so check whether or not it appears in the Core
test('T14137', [ check_errmsg(r'thunk') ], compile, ['-dsuppress-uniques -ddump-simpl'])
# bar and foo should CSEd here, so check for that in the Core
test('T14186', [ only_ways(['optasm']), check_errmsg(r'bar = foo') ], compile, ['-ddump-simpl'])

test('T14140',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T14140'])

test('T14272', normal, compile, [''])
test('T14270a', normal, compile, [''])
test('T14152', [ only_ways(['optasm']), check_errmsg(r'dead code') ], compile, ['-ddump-simpl'])
test('T14152a', [extra_files(['T14152.hs']), pre_cmd('cp T14152.hs T14152a.hs'),
                 only_ways(['optasm']), check_errmsg(r'dead code') ],
                compile, ['-fno-exitification -ddump-simpl'])
test('T13990', normal, compile, ['-dcore-lint -O'])
test('T14650', normal, compile, ['-O2'])
