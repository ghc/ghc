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

# Check if overloaded functions get unfoldings
test('ExposeOverloaded', [only_ways('[optasm]'), grep_errmsg('Unf=Unf')], compile, ['-ddump-simpl -dno-suppress-unfoldings'])

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
              	    ['-O2 -ddump-simpl -dsuppress-uniques -dsuppress-ticks -dno-typeable-binds'])
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
     js_skip, # Test prints NCG codegen info (LFI, etc.)
     makefile_test, ['T4201'])

test('T3772',
#      only_ways(['optasm']),
     normal,
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

test('T5298', [extra_files(['T5298A.hs', 'T5298B.hs'])], makefile_test, ['T5298'])
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
test('T5550', req_th, compile, [''])
test('T7865', normal, makefile_test, ['T7865'])

# T7785: we want to check that we specialise 'shared'. But Tidy discards the
# rule (see Note [Trimming auto-rules] in GHC.Iface.Tidy)
# So, rather arbitrarily, we dump the output of CSE and grep for SPEC
test('T7785', [ only_ways(['optasm']),
               grep_errmsg(r'SPEC') ],
      compile, ['-ddump-cse'])

test('T7702',
     [extra_files(['T7702plugin']),
      pre_cmd('$MAKE -s --no-print-directory -C T7702plugin package.T7702 TOP={top}'),
      # we say 18mb peak allocated +/- 70% because other compiler flags have
      # a large effect on allocation which is hard to separate from the
      # allocation done by the plugin... but a regression allocates > 90mb
      collect_compiler_stats('peak_megabytes_allocated',70),
      when(opsys('mingw32'), [multi_cpu_race]),
      req_interp
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
test('T8832', js_skip, makefile_test, ['T8832'])
test('T8848', normal, makefile_test, ['T8848'])
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
     js_broken(22261),
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

test('noinline01', only_ways(['optasm']), compile, ['-ddump-stg-from-core -dsuppress-uniques -dsuppress-ticks -O'])
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
test('T13156', normal, makefile_test, ['T13156'])
test('T11444', normal, compile, [''])
test('str-rules',
     normal,
     makefile_test, ['str-rules'])
test('T13170', only_ways(['optasm']), compile, ['-dcore-lint'])
test('T13317',
     normal,
     makefile_test, ['T13317'])
test('T13340', js_skip, makefile_test, ['T13340'])
test('T13338', only_ways(['optasm']), compile, ['-dcore-lint'])
test('T13367', normal, makefile_test, ['T13367'])
test('T13417', normal, compile, ['-O'])
test('T13413', normal, compile, [''])
test('T13410', normal, compile, ['-O2'])
test('T13468',
     normal,
     makefile_test, ['T13468'])
test('T13543', only_ways(['optasm']), compile, ['-ddump-dmd-signatures -ddump-cpr-signatures'])
test('T11272', normal, makefile_test, ['T11272'])
test('T12600', normal, makefile_test, ['T12600'])
test('T13157', normal, compile, ['-ddump-simpl -dno-typeable-binds -dsuppress-uniques -dsuppress-all -O'])
test('T13658', normal, compile, ['-dcore-lint'])
test('T14779a', normal, compile, ['-dcore-lint'])
test('T14779b', normal, compile, ['-dcore-lint'])
test('T13708', normal, compile, [''])
test('T14003', [only_ways(['optasm']), grep_errmsg('SC:')], compile, ['-ddump-spec-constr'])

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
test('T21694', [ req_profiling ] , compile, ['-O -prof -fprof-auto -funfolding-use-threshold=50 '])
test('T16978b', normal, compile, ['-O'])
test('T16979a', normal, compile, ['-O'])
test('T16979b', normal, compile, ['-O'])
test('T21577', normal, compile, ['-O'])
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
# NB: T17810: -fspecialise-aggressively
test('T17810', normal, multimod_compile, ['T17810', '-fspecialise-aggressively -dcore-lint -O -v0'])
test('T18013', normal, multimod_compile, ['T18013', '-v0 -O -dno-typeable-binds'])
test('T18098', normal, compile, ['-dcore-lint -O2'])
test('T18120', normal, compile, ['-dcore-lint -O'])

# Verify that there are only two top-level functions (the rec group of m's cast
# WW worker m1). Ideally, it would be one, but we fail to inline dead-ending
# recursive groups due to Note [Bottoming floats].
test('T18231', [ only_ways(['optasm']), grep_errmsg(r'^[\w\.]+ ::.*->.*') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=99999 -dsuppress-uniques'])

# Cast WW
test('T17673', [ only_ways(['optasm']), grep_errmsg(r'^\w+\.\$wf') ], compile, ['-ddump-simpl -dsuppress-uniques -dppr-cols=9999'])
test('T18078', [ only_ways(['optasm']), grep_errmsg(r'^\w+\.\$wf') ], compile, ['-ddump-simpl -dsuppress-uniques -dppr-cols=9999'])
test('T18328', [ only_ways(['optasm']), grep_errmsg(r'(Arity=2)',1) ], compile, ['-ddump-simpl -dsuppress-uniques'])
test('T18347', normal, compile, ['-dcore-lint -O'])
test('T18355', [ grep_errmsg(r'OneShot') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T18399', normal, compile, ['-dcore-lint -O'])
test('T18589', normal, compile, ['-dcore-lint -O'])
test('T18603', normal, compile, ['-dcore-lint -O'])

# T18649 should /not/ generate a specialisation rule
test('T18649', normal, compile, ['-O -ddump-rules -Wno-simplifiable-class-constraints'])

test('T18747A', normal, compile, [''])
test('T18747B', normal, compile, [''])
test('T18815', only_ways(['optasm']), makefile_test, ['T18815'])
test('T18668', normal, compile, ['-dsuppress-uniques'])
test('T18995', [ grep_errmsg(r'print') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T19168', req_th, compile, [''])
test('T19246', only_ways(['optasm']), multimod_compile, ['T19246', '-v0 -ddump-rules'])
test('T19360', only_ways(['optasm']), compile, [''])

# If the test goes wrong we'll get more case expressions in the output
test('T19581', [ grep_errmsg(r'case') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])

# T19586 should not generate missed specialisation warnings
test('T19586', normal, compile, [''])

test('T19599', normal, compile, ['-O -ddump-rules'])
test('T19599a', normal, compile, ['-O -ddump-rules'])

# Look for a specialisation rule for wimwam
test('T19672', normal, compile, ['-O2 -ddump-rules'])

test('T20103',  [ grep_errmsg(r'(Arity=[0-9]*)',[1]) ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T19780', normal, compile, ['-O2'])
test('T19794', normal, compile, ['-O'])
test('T19890',  [ grep_errmsg(r'= T19890.foo1') ], compile, ['-O -ddump-simpl'])
test('T20125',  [ grep_errmsg(r'= T20125.MkT') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T20143',  [ grep_errmsg(r'unsafeEqualityProof') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T20174', normal, compile, [''])
test('T16373', normal, compile, [''])
test('T20112',  normal, multimod_compile, ['T20112', '-O -v0 -g1'])
test('T20200', normal, compile, [''])
test('T20200a', normal, compile, ['-O2'])
test('T20200b', normal, compile, ['-O2'])
test('T20200KG', [extra_files(['T20200KGa.hs', 'T20200KG.hs-boot'])], multimod_compile, ['T20200KG', '-v0 -O2 -fspecialise-aggressively'])
test('T20639', normal, compile, ['-O2'])
test('T20894', normal, compile, ['-dcore-lint -O1 -ddebug-output'])
test('T19790',  normal, compile, ['-O -ddump-rule-firings'])

# This one had a Lint failure due to an occurrence analysis bug
# -O0 is needed to trigger it because that switches rules off,
# which (before the fix) lost crucial dependencies
test('T20820',  normal, compile, ['-O0'])

# Verify that the letrec is still there
test('T20895', [ grep_errmsg(r'\s*=\s*letrec') ], compile, ['-O0 -ddump-simpl -dsuppress-all -fno-local-float-out-top-level'])

test('OpaqueNoAbsentArgWW', [ grep_errmsg(r'$wf') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoCastWW', normal, compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoRebox', [ grep_errmsg(r'$wf') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoRebox2', [ grep_errmsg(r'$wf') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoRebox3', [ grep_errmsg(r'$wf') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoSpecConstr', [ req_th, grep_errmsg(r'$sloop') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoSpecialise', [ grep_errmsg(r'$sf') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoStrictArgWW', [ grep_errmsg(r'$wf') ], compile, ['-O -fworker-wrapper-cbv -ddump-simpl -dsuppress-uniques'])
test('OpaqueNoWW', [ grep_errmsg(r'$wf') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])

test('T21144',  normal, compile, ['-O'])

# Key here is that the argument to ifoldl' is eta-reduced in Core to
#   `/\m. f @(S m)`
# which will erase completely in STG
test('T20040', [ grep_errmsg(r'ifoldl\'') ], compile, ['-O -ddump-stg-final -dno-typeable-binds -dsuppress-all -dsuppress-uniques'])

# Key here is that yes* become visibly trivial due to eta-reduction, while no* are not eta-reduced.
test('T21261', [ grep_errmsg(r'^(yes|no)') ], compile, ['-O -ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques'])

# We expect to see a SPEC rule for $cm
test('T17966',  [ grep_errmsg(r'SPEC') ], compile, ['-O -ddump-spec'])
# We expect to see a SPEC rule for $cm
test('T19644',  [ grep_errmsg(r'SPEC') ], compile, ['-O -ddump-spec'])
test('T21391', normal, compile, ['-O -dcore-lint'])
# T22112: Simply test that dumping the Core doesn't loop becuse of the unfolding and ignore the dump output
test('T22112', [ grep_errmsg('never matches') ], compile, ['-O -dsuppress-uniques -dno-typeable-binds -fexpose-all-unfoldings -ddump-simpl'])
test('T21391a', normal, compile, ['-O -dcore-lint'])

# We don't want to see a thunk allocation for the insertBy expression after CorePrep.
test('T21392', [ grep_errmsg(r'sat.* :: \[\(.*Unique, .*Int\)\]') ], compile, ['-O -ddump-prep -dno-typeable-binds -dsuppress-uniques'])

test('T21689', [extra_files(['T21689a.hs'])], multimod_compile, ['T21689', '-v0 -O'])
test('T21801', normal, compile, ['-O -dcore-lint'])
test('T21848', [grep_errmsg(r'SPEC wombat') ], compile, ['-O -ddump-spec'])
test('T21694b', [grep_errmsg(r'Arity=4') ], compile, ['-O -ddump-simpl'])
test('T21960', [grep_errmsg(r'^ Arity=5') ], compile, ['-O2 -ddump-simpl'])
test('T21948', [grep_errmsg(r'^ Arity=5') ], compile, ['-O -ddump-simpl'])
test('DataToTagFamilyScrut', normal, compile, ['-O -ddump-simpl -dsuppress-all -dno-typeable-binds -dsuppress-uniques'])
test('T21763', only_ways(['optasm']), compile, ['-O2 -ddump-rules'])
test('T21763a', only_ways(['optasm']), compile, ['-O2 -ddump-rules'])
test('T22028', normal, compile, ['-O -ddump-rule-firings'])
test('T22114', normal, compile, ['-O'])
test('T21286',  normal, multimod_compile, ['T21286', '-O -ddump-rule-firings'])

# One module, T21851.hs, has OPTIONS_GHC -ddump-simpl
test('T21851', [grep_errmsg(r'case.*w\$sf') ], multimod_compile, ['T21851', '-O -dno-typeable-binds -dsuppress-uniques'])
# One module, T22097.hs, has OPTIONS_GHC -ddump-simpl
test('T22097', [grep_errmsg(r'case.*wgoEven') ], multimod_compile, ['T22097', '-O -dno-typeable-binds -dsuppress-uniques'])

test('T13873',  [ grep_errmsg(r'SPEC') ], compile, ['-O -ddump-rules -fpolymorphic-specialisation'])
test('T22357',  normal, compile, ['-O'])
test('T22471',  normal, compile, ['-O'])
test('T22347',  normal, compile, ['-O -fno-full-laziness'])
test('T22347a', normal, compile, ['-O2 -fno-full-laziness'])

# T17366: expecting to see a rule
#    Rule fired: SPEC/T17366 f @(Tagged tag) @_ (T17366)
test('T17366',  normal, multimod_compile, ['T17366', '-O -v0 -ddump-rule-firings -fpolymorphic-specialisation'])
test('T17366_AR',  [grep_errmsg(r'SPEC')], multimod_compile, ['T17366_AR', '-O -v0 -ddump-rule-firings -fpolymorphic-specialisation'])
test('T22375', normal, compile, ['-O -ddump-simpl -dsuppress-uniques -dno-typeable-binds -dsuppress-unfoldings'])
test('T22375DataFamily', normal, compile, ['-O -ddump-simpl -dsuppress-uniques -dno-typeable-binds -dsuppress-unfoldings'])

# One module, T21851_2.hs, has OPTIONS_GHC -ddump-simpl
# Expecting to see $s$wwombat
test('T21851_2', [grep_errmsg(r'wwombat') ], multimod_compile, ['T21851_2', '-O -dno-typeable-binds -dsuppress-uniques'])
# Should not inline m, so there shouldn't be a single YES
test('T22317', [grep_errmsg(r'ANSWER = YES') ], compile, ['-O -dinline-check m -ddebug-output'])
test('T22849', normal, compile, ['-O'])
test('T22634', normal, compile, ['-O -fcatch-nonexhaustive-cases'])
test('T22494', [grep_errmsg(r'case') ], compile, ['-O -ddump-simpl -dsuppress-uniques'])
test('T22491', normal, compile, ['-O2'])
test('T21476', normal, compile, [''])
test('T22272', normal, multimod_compile, ['T22272', '-O -fexpose-all-unfoldings -fno-omit-interface-pragmas -fno-ignore-interface-pragmas'])

# go should become a join point
test('T22428', [grep_errmsg(r'jump go') ], compile, ['-O -ddump-simpl -dsuppress-uniques -dno-typeable-binds -dsuppress-unfoldings'])

test('T22459', normal, compile, [''])
test('T22623', normal, multimod_compile, ['T22623', '-O -v0'])
test('T22662', normal, compile, [''])
test('T22725', normal, compile, ['-O'])
test('T22502', normal, compile, ['-O'])
test('T22611', [when(wordsize(32), skip), grep_errmsg(r'\$salterF') ], compile, ['-O -ddump-simpl -dsuppress-uniques -dsuppress-all -fpolymorphic-specialisation'])
test('T22715_2', normal, multimod_compile, ['T22715_2', '-v0 -O -fspecialise-aggressively'])
test('T22802', normal, compile, ['-O'])
test('T15205', normal, compile, ['-O -ddump-simpl -dno-typeable-binds -dsuppress-uniques'])
test('T22761', normal, multimod_compile, ['T22761', '-O2 -v0'])
test('T23630', normal, compile, ['-O'])
test('T23012', normal, compile, ['-O'])

test('RewriteHigherOrderPatterns', normal, compile, ['-O -ddump-rule-rewrites -dsuppress-all -dsuppress-uniques'])
test('T23024', normal, multimod_compile, ['T23024', '-O -v0'])
test('T23026', normal, compile, ['-O'])
test('T23083', [ grep_errmsg(r'eta.+::.+Integer') ], compile, ['-O -ddump-prep -dsuppress-uniques -dppr-cols=99999'])
test('T23267', [only_ways(['ghci-opt']), extra_hc_opts('-fspec-constr')], ghci_script, ['T23267.script'])
test('T23362', normal, compile, ['-O'])
test('T23307', normal, compile, ['-O -ddump-simpl -dno-typeable-binds -dsuppress-uniques'])
test('T23307a', normal, compile, ['-O -ddump-simpl -dno-typeable-binds -dsuppress-uniques'])
test('T23307b', normal, compile, ['-O'])
test('T23307c', normal, compile, ['-O'])
test('T22309', [grep_errmsg(r'MkW'), only_ways(['optasm']) ], compile, ['-O -ddump-stg-final -dsuppress-uniques -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures -dsuppress-module-prefixes'])
test('T23426', normal, compile, ['-O'])
test('T23491a', [extra_files(['T23491.hs']), grep_errmsg(r'Float out')], multimod_compile, ['T23491', '-ffull-laziness -ddump-full-laziness'])
test('T23491b', [extra_files(['T23491.hs']), grep_errmsg(r'Float inwards')], multimod_compile, ['T23491', '-ffloat-in -ddump-float-in'])
test('T23491c', [extra_files(['T23491.hs']), grep_errmsg(r'Liberate case')], multimod_compile, ['T23491', '-fliberate-case -ddump-liberate-case'])
test('T23491d', [extra_files(['T23491.hs']), grep_errmsg(r'Static argument')], multimod_compile, ['T23491', '-fstatic-argument-transformation -ddump-static-argument-transformation'])
test('T23074', normal, compile, ['-O -ddump-rules'])
test('T23272', [only_ways(['ghci']), extra_hc_opts('-fno-unoptimized-core-for-interpreter -O')], ghci_script, ['T23272.script'])
test('T23567', [extra_files(['T23567A.hs'])], multimod_compile, ['T23567', '-O -v0'])

# The -ddump-simpl of T22404 should have no let-bindings
test('T22404', [only_ways(['optasm']), check_errmsg(r'let') ], compile, ['-ddump-simpl -dsuppress-uniques'])
test('T23864', normal, compile, ['-O -dcore-lint -package ghc -Wno-gadt-mono-local-binds'])
test('T23938', [extra_files(['T23938A.hs'])], multimod_compile, ['T23938', '-O -v0'])
test('T23922a', normal, compile, ['-O'])
test('T23952', [extra_files(['T23952a.hs'])], multimod_compile, ['T23952', '-v0 -O'])
test('T24014', normal, compile, ['-dcore-lint'])
test('T24029', normal, compile, [''])
test('T21348', normal, compile, ['-O'])
test('T21917', normal, compile, ['-O -fkeep-auto-rules -ddump-rules'])
test('T23209', [extra_files(['T23209_Aux.hs'])], multimod_compile, ['T23209', '-v0 -O'])
test('T24229a', [ grep_errmsg(r'wfoo') ], compile, ['-O2 -ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques -dppr-cols=99999'])
test('T24229b', [ grep_errmsg(r'wfoo') ], compile, ['-O2 -ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques -dppr-cols=99999'])
test('T24370', normal, compile, ['-O'])
test('T24551', normal, compile, ['-O -dcore-lint'])
test('T24726', normal, compile, ['-dcore-lint -dsuppress-uniques'])
test('T24768', normal, compile, ['-O'])
test('T24770', [ grep_errmsg(r'Dead') ], compile, ['-O'])
test('T24808', [ grep_errmsg(r'myFunction') ], compile, ['-O -ddump-simpl'])

# T24944 needs -O2 because it's about SpecConstr
test('T24944', [extra_files(['T24944a.hs'])], multimod_compile, ['T24944', '-v0 -O2'])
