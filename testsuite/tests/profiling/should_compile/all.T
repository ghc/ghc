# We need to run prof001 and prof002 the normal way, as the extra flags
# added for the profiling ways makes it pass
test('prof001', [only_ways(['normal']), req_profiling], compile_and_run, ['-prof -fprof-cafs'])
test('prof002', [only_ways(['normal']), req_profiling], compile_and_run, ['-prof -fprof-cafs'])
test('prof-late-cc', [only_ways(['normal']), req_profiling], compile, ['-prof -fprof-late-ccs'])
test('prof-late-cc2', [only_ways(['normal']), req_profiling], compile_and_run, ['-prof -fprof-late-ccs'])

test('T2410', [only_ways(['normal']), req_profiling], compile, ['-O2 -prof -fprof-cafs'])
test('T5889', [only_ways(['normal']), req_profiling, extra_files(['T5889/A.hs', 'T5889/B.hs'])], multimod_compile, ['A B', '-O -prof -fno-prof-count-entries -v0'])
test('T12790', [only_ways(['normal']), req_profiling], compile, ['-O -prof'])
test('T14931', [only_ways(['normal']), req_profiling, unless(have_dynamic(), skip)],
     makefile_test, ['T14931'])
test('T15108', [only_ways(['normal']), req_profiling], compile, ['-O -prof -fprof-auto'])
test('T19894', [only_ways(['normal']), req_profiling, extra_files(['T19894'])], multimod_compile, ['Main', '-v0 -O2 -prof -fprof-auto -iT19894'])
test('T20938', [only_ways(['normal']), req_profiling], compile, ['-O -prof'])
