prun_ways = (['prof_no_auto'] if have_profiling() else []) + (['profdyn_no_auto'] if have_dynamic_prof() else [])

test_opts = [ only_ways(['prof_no_auto', 'profdyn'])
            , extra_ways(prun_ways) if prun_ways else skip]

# We need to run prof001 and prof002 the normal way, as the extra flags
# added for the profiling ways makes it pass
test('prof001', [test_opts], compile_and_run, ['-prof -fprof-cafs'])
test('prof002', [test_opts], compile_and_run, ['-prof -fprof-cafs'])
test('prof-late-cc', [test_opts], compile, ['-prof -fprof-late'])
test('prof-late-cc2', [test_opts], compile_and_run, ['-prof -fprof-late'])
test('prof-late-cc3', [test_opts, grep_errmsg('scc')], compile, ['-prof -fprof-late -ddump-prep -ddump-simpl -dno-typeable-binds'])
test('prof-late-cc4', [test_opts, grep_errmsg('scc')], compile, ['-prof -fprof-late-inline -ddump-simpl -dno-typeable-binds -O'])

test('T2410', [test_opts], compile, ['-O2 -prof -fprof-cafs'])
test('T5889', [test_opts, extra_files(['T5889/A.hs', 'T5889/B.hs'])], multimod_compile, ['A B', '-O -prof -fno-prof-count-entries -v0'])
test('T12790', [test_opts], compile, ['-O -prof'])
test('T14931', [test_opts, unless(have_dynamic(), skip)],
     makefile_test, ['T14931'])
test('T15108', [test_opts], compile, ['-O -prof -fprof-auto'])
test('T19894', [test_opts, extra_files(['T19894'])], multimod_compile, ['Main', '-v0 -O2 -prof -fprof-auto -iT19894'])
test('T20938', [test_opts], compile, ['-O -prof'])
