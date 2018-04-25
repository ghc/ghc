# We need to run prof001 and prof002 the normal way, as the extra flags
# added for the profiling ways makes it pass
test('prof001', [only_ways(['normal']), req_profiling], compile_and_run, ['-prof -fprof-cafs'])
test('prof002', [only_ways(['normal']), req_profiling], compile_and_run, ['-prof -fprof-cafs'])

test('T2410', [only_ways(['normal']), req_profiling], compile, ['-O2 -prof -fprof-cafs'])
test('T5889', [expect_broken(5889), only_ways(['normal']), req_profiling, extra_files(['T5889/A.hs', 'T5889/B.hs'])], multimod_compile, ['A B', '-O -prof -fno-prof-count-entries -v0'])
