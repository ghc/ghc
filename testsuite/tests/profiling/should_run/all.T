# Test for #1227, #1418

extra_prof_ways = ['prof', 'prof_hc_hb', 'prof_hb', 'prof_hd', 'prof_hy', 'prof_hr']

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

test('scc001', [req_profiling,
                extra_ways(['prof']), only_ways(['prof'] + prof_ways)],
     compile_and_run,
     ['-fno-state-hack']) # Note [consistent stacks]

test('scc002', [req_profiling,
                extra_ways(['prof']), only_ways(['prof'] + prof_ways)],
     compile_and_run,
     [''])

test('scc003', [req_profiling,
                extra_ways(['prof']), only_ways(['prof'] + prof_ways)],
     compile_and_run,
     ['-fno-state-hack']) # Note [consistent stacks]

test('5314',
     [ only_ways(prof_ways + extra_prof_ways),
       extra_ways(extra_prof_ways),
       req_profiling ],
     compile_and_run,
     [''])

test('T680',
     [ req_profiling, extra_ways(['prof']), only_ways(['prof'] + prof_ways) ],
     compile_and_run,
     ['-fno-full-laziness']) # Note [consistent stacks]

test('T2552',
     [ req_profiling, extra_ways(['prof']), only_ways(['prof'] + prof_ways) ],
     compile_and_run,
     [''])

test('T949',
     [ req_profiling, extra_ways(extra_prof_ways), only_ways(extra_prof_ways) ],
     compile_and_run,
     [''])

test('ioprof',
     [ req_profiling, extra_ways(['prof']), only_ways(['prof'] + prof_ways),
       exit_code(1) ],
     compile_and_run,
     ['-fno-full-laziness -fno-state-hack']) # Note [consistent stacks]

# These two examples are from the User's Guide:

test('prof-doc-fib',
     [ req_profiling, extra_ways(['prof']), only_ways(['prof'] + prof_ways) ],
     compile_and_run,
     [''])

test('prof-doc-last',
     [ req_profiling, extra_ways(['prof']), only_ways(['prof'] + prof_ways) ],
     compile_and_run,
     [''])

# Note [consistent stacks]
#  Certain optimisations can change the stacks we get out of the
#  profiler.  These flags are necessary (but perhaps not sufficient)
#  to get consistent stacks:
#
#       -fno-state-hack
#       -fno-full-laziness

