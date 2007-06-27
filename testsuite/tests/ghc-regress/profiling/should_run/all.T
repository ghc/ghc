# Test for #1227, #1418

prof_ways = ['prof_hc_hb', 'prof_hb', 'prof_hd', 'prof_hy', 'prof_hr']

test('heapprof001',
     composes([only_ways(['profasm','profc'] + prof_ways),
               extra_ways(prof_ways),
               req_profiling,
               extra_run_opts('7')]),
     compile_and_run, [''])
