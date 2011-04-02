# Test for #1227, #1418

extra_prof_ways = ['prof_hc_hb', 'prof_hb', 'prof_hd', 'prof_hy', 'prof_hr']

test('heapprof001',
     composes([only_ways(prof_ways + extra_prof_ways),
               extra_ways(extra_prof_ways),
               req_profiling,
               extra_run_opts('7')]),
     compile_and_run, [''])

test('2592',
     [only_ways(['profasm']), req_profiling,
      extra_run_opts('+RTS -M1m -RTS'), exit_code(251)],
     compile_and_run, [''])

test('T3001',
     [only_ways(['prof_hb']), extra_ways(['prof_hb']), req_profiling],
     compile_and_run, [''])

test('T3001-2',
     [only_ways(['prof_hb']), extra_ways(['prof_hb']), req_profiling],
     compile_and_run, ['-package bytestring'])

test('scc001', [expect_broken(4414), req_profiling],
     run_command, ['$MAKE -s --no-print-directory scc001'])

