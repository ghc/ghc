# Test for #1227, #1418

test('heapprof002',
     [extra_files(['heapprof001.hs']),
      pre_cmd('cp heapprof001.hs heapprof002.hs'), extra_ways(['normal_h']),
      extra_run_opts('7')],
     compile_and_run, [''])

test('T11489', [req_profiling], run_command,
     ['$MAKE -s --no-print-directory T11489'])

# Below this line, run tests only with profiling ways.
setTestOpts(req_profiling)
setTestOpts(extra_ways(['prof', 'ghci-ext-prof']))
setTestOpts(only_ways(prof_ways))
setTestOpts(keep_prof_callstacks)

extra_prof_ways = ['prof', 'prof_hc_hb', 'prof_hb', 'prof_hd', 'prof_hy', 'prof_hr']

expect_broken_for_10037 = expect_broken_for(
  10037,
  [w for w in prof_ways if w not in opt_ways and w != 'ghci-ext-prof'])
     # e.g. prof and profllvm

test('heapprof001',
     [when(have_profiling(), extra_ways(extra_prof_ways)), extra_run_opts('7')],
     compile_and_run, [''])

test('T2592',
     [only_ways(['profasm']), extra_run_opts('+RTS -M1m -RTS'), exit_code(251)],
     compile_and_run, [''])

test('T3001', [only_ways(['prof_hb']), extra_ways(['prof_hb'])],
     compile_and_run, [''])

test('T3001-2',
     [only_ways(['prof_hb']), extra_ways(['prof_hb'])],
     compile_and_run, ['-package bytestring'])

# For profasm/profthreaded, the answer is correct but the ordering of some
# lines in the output happen to be different, so we just skip them.
test('scc001',
     omit_ways(['profasm', 'profthreaded']),
     compile_and_run,
     ['-fno-state-hack -fno-full-laziness']) # Note [consistent stacks]

test('scc002', [], compile_and_run, [''])

test('scc003', [], compile_and_run,
     ['-fno-state-hack']) # Note [consistent stacks]

test('T5654-O0', [only_ways(['prof'])], compile_and_run, [''])

test('T5654-O1', [only_ways(['profasm'])], compile_and_run, [''])

test('T5654b-O0', [only_ways(['prof'])], compile_and_run, [''])

test('T5654b-O1', [only_ways(['profasm'])], compile_and_run, [''])

test('scc005', [], compile_and_run, [''])

test('T5314', [extra_ways(extra_prof_ways)], compile_and_run, [''])

test('T680', [], compile_and_run,
     ['-fno-full-laziness']) # Note [consistent stacks]

test('T2552', [expect_broken_for_10037], compile_and_run, [''])

test('T949', [extra_ways(extra_prof_ways)], compile_and_run, [''])

# The results for 'prof' are fine, but the ordering changes.
# We care more about getting the optimised results right, so ignoring
# this for now.
test('ioprof',
     [expect_broken_for_10037,
      exit_code(1),
      omit_ways(['ghci-ext-prof']) # doesn't work with exit_code(1)
     ],
     compile_and_run,
     ['-fno-full-laziness -fno-state-hack']) # Note [consistent stacks]

# These two examples are from the User's Guide:

test('prof-doc-fib', [], compile_and_run, [''])

test('prof-doc-last', [], compile_and_run, ['-fno-full-laziness'])

# unicode in cost centre names
test('T5559', [], compile_and_run, [''])

# Note [consistent stacks]
#  Certain optimisations can change the stacks we get out of the
#  profiler.  These flags are necessary (but perhaps not sufficient)
#  to get consistent stacks:
#
#       -fno-state-hack
#       -fno-full-laziness

test('callstack001',
     # unoptimised results are different w.r.t. CAF attribution
     [ omit_ways(['ghci-ext-prof']), # produces a different stack
     ], compile_and_run,
     ['-fprof-auto-calls -fno-full-laziness -fno-state-hack'])

test('callstack002',
     [ omit_ways(['ghci-ext-prof']), # produces a different stack
     ],
     compile_and_run,
     ['-fprof-auto-calls -fno-full-laziness -fno-state-hack'])

# Should not stack overflow with -prof -fprof-auto
test('T5363', [], compile_and_run, [''])

test('profinline001', [], compile_and_run, [''])

# Skip T11627a and T11627b on Darwin as it tends to give us a very small stack
# which the retainer profiler tends to overflow. See #15287 and #11627.
test('T11627a', [ extra_ways(extra_prof_ways)
                , when(opsys('darwin'), skip)
                ],
     compile_and_run, [''])

test('T11627b', [ extra_run_opts('+RTS -i0 -RTS')  # census after each GC
                , extra_ways(extra_prof_ways)
                , when(opsys('darwin'), skip)
                , when(opsys('mingw32'),
                       expect_broken_for(12236, ['prof_hc_hb']))
                ]
                , compile_and_run
                , [''])

test('T11978a',
     [only_ways(['profthreaded']),
      extra_run_opts('+RTS -hb -N10'),
      expect_broken(12019)],
     compile_and_run, [''])

test('toplevel_scc_1',
     [extra_ways(['prof_no_auto']), only_ways(['prof_no_auto'])],
     compile_and_run,
     [''])

test('T12962', [], compile_and_run, [''])

test('T14257', [], run_command, ['$MAKE -s --no-print-directory T14257'])
