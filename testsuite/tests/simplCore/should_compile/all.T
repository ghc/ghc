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
test('simpl016', normal, compile, ['-dsuppress-uniques -dsuppress-ticks'])
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
test('rule2', only_ways(['optasm']), compile, ['-dsuppress-uniques -dsuppress-ticks'])
test('T7162', normal, compile, [''])

test('dfun-loop', normal, compile, [''])
test('strict-float', normal, compile, [''])
test('T3118', normal, compile, ['-Wno-overlapping-patterns'])
test('T4203', normal, compile, [''])

# With -prof -fvia-C, this test makes gcc go out to lunch, and the
# testsuite driver times out.
test('T3016', when(fast(), skip), compile, [''])

test('T1647', normal, compile, ['-fdicts-strict -dcore-lint'])

test('T3234',
     normal,
     makefile_test, ['T3234'])

test('T3717', only_ways(['optasm']),
              compile,
              ['-ddump-simpl -dsuppress-uniques -dsuppress-ticks'])

test('spec-inline', only_ways(['optasm']),
              	    compile,
              	    ['-O2 -ddump-simpl -dsuppress-uniques -dsuppress-ticks'])
test('T4908', only_ways(['optasm']),
              compile,
              ['-O2 -ddump-simpl -dsuppress-uniques -dsuppress-ticks'])

test('T4930', only_ways(['optasm']),
              compile,
              ['-O -ddump-simpl -dsuppress-uniques -dsuppress-ticks'])

test('T3055', [], makefile_test, ['T3055'])

test('T4306',
     normal,
     makefile_test, ['T4306'])

test('T4201',
     normal,
     makefile_test, ['T4201'])

test('T3772',
#      only_ways(['optasm']),
     extra_clean(['T3772_A.hi', 'T3772_A.o']),
     makefile_test, ['T3772'])

test('EvalTest',
     normal,
     makefile_test, ['EvalTest'])

test('T3831', normal, compile, [''])
test('T4345', normal, compile, [''])
test('T4398', normal, compile, ['-dsuppress-uniques -dsuppress-ticks'])

test('T4903', [], makefile_test, ['T4903'])

test('T4918', [], makefile_test, ['T4918'])

# This test flips too and fro about whether it passes
# I'm not sure why it is so delicate, but it greps the
# result of -ddump-simpl, which is never advertised to
# be very stable
test('T4945',
     normal,
     makefile_test, ['T4945'])

test('T4957',
     normal,
     makefile_test, ['T4957'])

test('T5168',
     normal,
     makefile_test, ['T5168'])

test('T5329', normal, compile, [''])
test('T5303', normal, compile, [''])   # Coercion-optimisation test
test('T5342', normal, compile, [''])   # Lint error with -prof
test('T5359a', normal, compile, [''])  # Lint error with -O (OccurAnal)
test('T5359b', normal, compile, [''])  # Lint error with -O (OccurAnal)
test('T5458', normal, compile, [''])
test('simpl021', [extra_files(['Simpl021A.hs', 'Simpl021B.hs'])], makefile_test, ['simpl021'])
test('T5327', normal, makefile_test, ['T5327'])
test('T16254', normal, makefile_test, ['T16254'])
test('T5615', normal, makefile_test, ['T5615'])
test('T5623', normal, makefile_test, ['T5623'])
test('T13155', normal, makefile_test, ['T13155'])
test('T5658b',
     normal,
     makefile_test, ['T5658b'])
test('T5776',
     normal,
     makefile_test, ['T5776'])
test('T4138', [], makefile_test, ['T4138'])
test('T7088', normal, compile, [''])
test('T6082-RULE', normal, compile, [''])
test('T7165', [], makefile_test, ['T7165'])
test('T7287', normal, compile, [''])
test('T7360', only_ways(['optasm']), compile, ['-ddump-simpl -dsuppress-uniques -dsuppress-ticks'])
test('T5366',
     normal,
     makefile_test, ['T5366'])
test('T7796', [], makefile_test, ['T7796'])
test('T5550', omit_ways(prof_ways), compile, [''])
test('T7865', normal, makefile_test, ['T7865'])
test('T7785', only_ways(['optasm']), compile, ['-ddump-rules'])
test('T7702',
     [extra_files(['T7702plugin']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C T7702plugin package.T7702 TOP={top}'),
      # we say 18mb peak allocated +/- 70% because other compiler flags have
      # a large effect on allocation which is hard to separate from the
      # allocation done by the plugin... but a regression allocates > 90mb
      collect_compiler_stats('peak_megabytes_allocated',70),
      when(opsys('mingw32'), fragile_for(16799, ['normal']))
     ],
     compile,
     ['-v0 -package-db T7702plugin/pkg.T7702/local.package.conf -fplugin T7702Plugin -package T7702plugin ' + config.plugin_way_flags])

# we omit profasm because it doesn't bring much to the table but
# introduces its share of complexity, as the test as it is fails with
# profasm:
# T7944.hs:7:1: fatal:
#     Cannot load -prof objects when GHC is built with -dynamic
#     To fix this, either:
#       (1) Use -fexternal-interpreter, or
#       (2) Build the program twice: once with -dynamic, and then
#           with -prof using -osuf to set a different object file suffix.
test('T7944', omit_ways(['profasm']), compile, ['-O2'])

test('T8196', normal, compile, [''])
test('T8221b', normal, compile, [''])
test('T8221', [], makefile_test, ['T8221'])
test('T8274', normal, makefile_test, ['T8274'])
test('T8329', only_ways(['optasm']), multimod_compile, ['T8329','-v0 -O'])
test('T5996',
     normal,
     makefile_test, ['T5996'])
test('T8537', normal, compile, [''])
test('T8832',
     normal,
     run_command, ['$MAKE -s --no-print-directory T8832 T8832_WORDSIZE_OPTS=' +
      ('-DT8832_WORDSIZE_64' if wordsize(64) else '')])
test('T8848', normal,
     makefile_test, ['T8848'])
test('T8848a', only_ways(['optasm']), compile, ['-ddump-rules'])
test('T8331', only_ways(['optasm']), compile, ['-ddump-rules'])
test('T6056', only_ways(['optasm']), multimod_compile, ['T6056', '-v0 -ddump-rule-firings'])
test('T9400', only_ways(['optasm']), compile, ['-O0 -ddump-simpl -dsuppress-uniques -dsuppress-ticks'])
test('T9441a', [only_ways(['optasm']), check_errmsg(r'f1 = f2') ], compile, ['-ddump-simpl -dsuppress-ticks'])
test('T9441b', [only_ways(['optasm']), check_errmsg(r'Rec {') ], compile, ['-ddump-simpl'])
test('T9441c', [only_ways(['optasm']), check_errmsg(r'Rec {') ], compile, ['-ddump-simpl'])
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
     makefile_test, ['T10083'])
test('T10689', normal, compile, [''])
test('T11155',
     normal,
     makefile_test, ['T11155'])
test('T11232', normal, compile, ['-O2'])
test('T11562', normal, compile, ['-O2'])
test('T11742', normal, compile, ['-O2'])
test('T11644', normal, compile, ['-O2'])
test('T3990',
     normal,
     makefile_test, ['T3990'])

test('T12076', [], multimod_compile, ['T12076', '-v0'])
test('T12076lit', normal, compile, ['-O'])
test('T12076sat', normal, compile, ['-O'])
test('T12212', normal, compile, ['-O'])

test('noinline01', only_ways(['optasm']), compile, ['-ddump-stg -dsuppress-uniques -dsuppress-ticks -O'])
test('par01', only_ways(['optasm']), compile, ['-ddump-prep -dsuppress-uniques -dsuppress-ticks -O2'])
test('T12776', normal, compile, ['-O2'])
test('T9509',
     normal,
     makefile_test, ['T9509'])
test('T12603',
     normal,
     makefile_test, ['T12603'])
test('T12877', normal, makefile_test, ['T12877'])
test('T13027', normal, compile, [''])
test('T13025',
     normal,
     makefile_test, ['T13025'])
test('T13143', only_ways(['optasm']), compile, ['-O -ddump-simpl -dsuppress-uniques -dsuppress-ticks'])
test('T13156', normal, makefile_test, ['T13156'])
test('T11444', normal, compile, [''])
test('str-rules',
     normal,
     makefile_test, ['str-rules'])
test('T13170', only_ways(['optasm']), compile, ['-dcore-lint'])
test('T13317',
     normal,
     makefile_test, ['T13317'])
test('T13340', normal, makefile_test, ['T13340'])
test('T13338', only_ways(['optasm']), compile, ['-dcore-lint'])
test('T13367', normal, makefile_test, ['T13367'])
test('T13417', normal, compile, ['-O'])
test('T13413', normal, compile, [''])
test('T13410', normal, compile, ['-O2'])
test('T13468',
     normal,
     makefile_test, ['T13468'])
test('T13543', only_ways(['optasm']), compile, ['-ddump-str-signatures -ddump-cpr-signatures'])
test('T11272', normal, makefile_test, ['T11272'])
test('T12600', normal, makefile_test, ['T12600'])
test('T13658', normal, compile, ['-dcore-lint'])
test('T14779a', normal, compile, ['-dcore-lint'])
test('T14779b', normal, compile, ['-dcore-lint'])
test('T13708', normal, compile, [''])

# thunk should inline here, so check whether or not it appears in the Core
# (we skip profasm because it might not inline there)
test('T14137', [ check_errmsg(r'thunk'), omit_ways(['profasm']) ], compile, ['-dsuppress-uniques -dsuppress-ticks -ddump-simpl'])
# bar and foo should CSEd here, so check for that in the Core
test('T14186', [ only_ways(['optasm']), check_errmsg(r'bar = foo') ], compile, ['-ddump-simpl -dsuppress-ticks'])

test('T14140',
     normal,
     makefile_test, ['T14140'])

test('T14272', normal, compile, [''])
test('T14270a', normal, compile, [''])
test('T14152', [ only_ways(['optasm']), check_errmsg(r'dead code') ], compile, ['-ddump-simpl'])
test('T14152a', [extra_files(['T14152.hs']), pre_cmd('cp T14152.hs T14152a.hs'),
                 only_ways(['optasm']), check_errmsg(r'dead code') ],
                compile, ['-fno-exitification -ddump-simpl'])
test('T13990', normal, compile, ['-dcore-lint -O'])
test('T14650', normal, compile, ['-O2'])
test('T14959', normal, compile, ['-O'])
test('T14978',
     normal,
     makefile_test, ['T14978'])
test('T15002', [ req_profiling ], compile, ['-O -fprof-auto -prof'])
test('T15005', normal, compile, ['-O'])
# we omit profiling because it affects the optimiser and makes the test fail
test('T15056', [extra_files(['T15056a.hs']), omit_ways(['profasm'])], multimod_compile, ['T15056', '-O -v0 -ddump-rule-firings'])
test('T15186', normal, multimod_compile, ['T15186', '-v0'])
test('T15517', normal, compile, ['-O0'])
test('T15517a', normal, compile, ['-O0'])
test('T15453', normal, compile, ['-dcore-lint -O1'])
test('T15445', normal, multimod_compile, ['T15445', '-v0 -O -ddump-rule-firings'])
test('T15631',
     normal,
     makefile_test, ['T15631'])
test('T15673', normal, compile, ['-O'])
test('T16288', normal, multimod_compile, ['T16288B', '-O -dcore-lint -v0'])
test('T16348', normal, compile, ['-O'])
test('T16918', normal, compile, ['-O'])
test('T16918a', normal, compile, ['-O'])
test('T16978a', normal, compile, ['-O'])
test('T16978b', normal, compile, ['-O'])
test('T16979a', normal, compile, ['-O'])
test('T16979b', normal, compile, ['-O'])
test('T17140',
     [extra_files(['T17140a.hs'])],
     makefile_test,
     ['T17140'])
test('T17409',
     normal,
     makefile_test, ['T17409'])
test('T17429', normal, compile, ['-dcore-lint -O2'])
test('T17590', normal, compile, ['-dcore-lint -O2'])
test('T17722', normal, multimod_compile, ['T17722B', '-dcore-lint -O2 -v0'])
test('T17724', normal, compile, ['-dcore-lint -O2'])
test('T17787',  [ grep_errmsg(r'foo') ], compile, ['-ddump-simpl -dsuppress-uniques'])
test('T17901',
     normal,
     makefile_test, ['T17901'])
test('T17930', [ grep_errmsg(r'^\$sfoo') ], compile, ['-O -ddump-spec -dsuppress-uniques -dsuppress-idinfo'])
test('spec004', [ grep_errmsg(r'\$sfoo') ], compile, ['-O -ddump-spec -dsuppress-uniques'])
test('T17966',
     normal,
     makefile_test, ['T17966'])
# NB: T17810: -fspecialise-aggressively
test('T17810', normal, multimod_compile, ['T17810', '-fspecialise-aggressively -dcore-lint -O -v0'])
test('T18013', normal, multimod_compile, ['T18013', '-v0 -O'])
test('T18098', normal, compile, ['-dcore-lint -O2'])
test('T18120', normal, compile, ['-dcore-lint -O'])

# Verify that there are only two top-level functions (the rec group of m's cast
# WW worker m1). Ideally, it would be one, but we fail to inline dead-ending
# recursive groups due to Note [Bottoming floats].
test('T18231', [ only_ways(['optasm']), grep_errmsg(r'^[\w\.]+ ::.*->.*') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=99999 -dsuppress-uniques'])
