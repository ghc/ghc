setTestOpts(js_skip) # JS backend doesn't support profiling yet

prun_ways = (['prof'] if have_profiling() else []) + (['profdyn'] if have_dynamic_prof() else [])

# Options to use when comparing .prof files
test_opts_dot_prof = [ only_ways(['prof', 'profdyn'])
            , extra_ways(prun_ways) if prun_ways else skip]

# Test for #1227, #1418
test('heapprof002',
     [extra_files(['heapprof001.hs']),
      when(not(wasm_arch()),ignore_extension),
      pre_cmd('cp heapprof001.hs heapprof002.hs'), extra_ways(['normal_h']),
      extra_run_opts('7')],
     compile_and_run, [''])

test('T11489', [req_profiling], makefile_test, ['T11489'])

test('dynamic-prof', [], compile_and_run, [''])

test('dynamic-prof2', [only_ways(['normal', 'nonmoving_thr']), extra_run_opts('+RTS -hT --no-automatic-heap-samples')], compile_and_run, [''])

test('dynamic-prof3', [only_ways(['normal', 'nonmoving_thr']), extra_run_opts('+RTS -hT --no-automatic-heap-samples')], compile_and_run, [''])

# Remove the ipName field as it's volatile (depends on e.g. architecture and may change with every new GHC version)
def normalise_InfoProv_ipName(str):
     return re.sub('ipName = "\\w*"', '', str)

test('staticcallstack001',
     [ omit_ways(ghci_ways + ['ghci-ext-prof']), # produces a different stack
       normalise_fun(normalise_InfoProv_ipName)
     ], compile_and_run,
     ['-O0 -g3 -fdistinct-constructor-tables -finfo-table-map'])

test('staticcallstack002',
     [ omit_ways(ghci_ways + ['ghci-ext-prof']), # produces a different stack
       normalise_fun(normalise_InfoProv_ipName)
     ], compile_and_run,
     ['-O0 -g3 -fdistinct-constructor-tables -finfo-table-map'])

test('T21455',
     [extra_run_opts('+RTS -hT -postem')],
     compile_and_run,
     [''])


# Below this line, run tests only with profiling ways.
prun_ways = (['prof', 'ghci-ext-prof'] if have_profiling() else []) + (['profdyn'] if have_dynamic_prof() else [])
test_opts = [ extra_ways(prun_ways) if prun_ways else skip ]
setTestOpts(test_opts)
setTestOpts(only_ways(prof_ways))
setTestOpts(keep_prof_callstacks)

extra_prof_ways = ['prof', 'prof_hc_hb', 'prof_hb', 'prof_hd', 'prof_hy', 'prof_hr']

expect_broken_for_10037 = expect_broken_for(
  10037,
  [w for w in prof_ways if w not in opt_ways and w != 'ghci-ext-prof'])
     # e.g. prof and profllvm

test('heapprof001',
     [when(have_profiling(), extra_ways(extra_prof_ways)),
      fragile(15382),
      extra_run_opts('7')],
     compile_and_run, [''])

test('T2592',
     [only_ways(['profasm']), extra_run_opts('+RTS -M1m -A1m -RTS'),
     exit_code(1 if arch('wasm32') else 251),
     when(arch('wasm32'), ignore_stderr)],
     compile_and_run, [''])

test('T3001', [only_ways(['prof_hb']), extra_ways(['prof_hb'])],
     compile_and_run, [''])

test('T3001-2',
     [only_ways(['prof_hb']), extra_ways(['prof_hb'])],
     compile_and_run, ['-package bytestring'])

# For profasm/profthreaded, the answer is correct but the ordering of some
# lines in the output happen to be different, so we just skip them.
test('scc001',
     [grep_prof("scc001.hs"), omit_ways(['profasm', 'profthreaded'])],
     compile_and_run,
     ['-fno-state-hack -fno-full-laziness']) # Note [consistent stacks]

test('scc002', [grep_prof("scc002.hs"), test_opts_dot_prof], compile_and_run, [''])

test('scc003', [grep_prof("scc003.hs"), test_opts_dot_prof], compile_and_run,
     ['-fno-state-hack']) # Note [consistent stacks]

test('T5654-O0', [only_ways(['prof']), grep_prof("T5654-O0.hs")], compile_and_run, [''])

test('T5654-O1', [only_ways(['profasm']), grep_prof("T5654-O1.hs")], compile_and_run, [''])

test('T5654b-O0', [only_ways(['prof']), grep_prof("T5654b-O0.hs")], compile_and_run, [''])

test('T5654b-O1', [only_ways(['profasm']), grep_prof("T5654b-O1.hs")], compile_and_run, [''])

test('scc005', [grep_prof("scc005.hs"), test_opts_dot_prof], compile_and_run, ['-fno-worker-wrapper'])

test('T5314', [extra_ways(extra_prof_ways)], compile_and_run, [''])

test('T680', [test_opts_dot_prof, grep_prof("T680.hs")], compile_and_run,
     ['-fno-full-laziness']) # Note [consistent stacks]

test('T2552', [test_opts_dot_prof, grep_prof("T2552.hs")], compile_and_run, [''])

test('T949', [extra_ways(extra_prof_ways)], compile_and_run, [''])

# The results for 'prof' are fine, but the ordering changes.
# We care more about getting the optimised results right, so ignoring
# this for now.
test('ioprof',
     [normal,
      exit_code(1),
      test_opts_dot_prof,
      grep_prof("ioprof.hs"),
      omit_ways(['ghci-ext-prof']), # doesn't work with exit_code(1)
      ignore_stderr
     ],
     compile_and_run,
     ['-fno-full-laziness -fno-state-hack']) # Note [consistent stacks]

# These two examples are from the User's Guide:

test('prof-doc-fib', [grep_prof("prof-doc-fib.hs"), test_opts_dot_prof], compile_and_run, [''])

test('prof-doc-last', [grep_prof("prof-doc-last.hs"), test_opts_dot_prof], compile_and_run, ['-fno-full-laziness'])

# unicode in cost centre names
test('T5559', [test_opts_dot_prof, fragile(16350), grep_prof("T5559.hs")], compile_and_run, [''])

test('T7275', test_opts_dot_prof, makefile_test, [])

# Note [consistent stacks]
# ~~~~~~~~~~~~~~~~~~~~~~~~
#  Certain optimisations can change the stacks we get out of the
#  profiler.  These flags are necessary (but perhaps not sufficient)
#  to get consistent stacks:
#
#       -fno-state-hack
#       -fno-full-laziness

test('callstack001',
     # unoptimised results are different w.r.t. CAF attribution
     [test_opts_dot_prof # produces a different stack
     ], compile_and_run,
     ['-fprof-auto-calls -fno-full-laziness -fno-state-hack'])

test('callstack002',
     [ test_opts_dot_prof # produces a different stack
     ],
     compile_and_run,
     ['-fprof-auto-calls -fno-full-laziness -fno-state-hack'])

# Should not stack overflow with -prof -fprof-auto
test('T5363', [], compile_and_run, [''])

test('profinline001', [grep_prof("profinline001.hs"), test_opts_dot_prof], compile_and_run, [''])

test('T11627a', [ extra_ways(extra_prof_ways),
                  # Slow bignum implementations timeout
                  when(have_slow_bignum(), skip)
                ],
     compile_and_run, [''])

test('T11627b', [ extra_run_opts('+RTS -i0 -RTS')  # census after each GC
                , extra_ways(extra_prof_ways)
                , when(opsys('darwin'), skip)
                , when(opsys('mingw32'), fragile_for(12236, ['prof_hc_hb']))
                ]
                , compile_and_run
                , [''])

test('T11978a',
     [only_ways(['profthreaded']),
      extra_run_opts('+RTS -hb -N10'),
      expect_broken(12019)],
     compile_and_run, [''])

test('toplevel_scc_1',
     [grep_prof("toplevel_scc_1.hs"), extra_ways(['prof_no_auto']), only_ways(['prof_no_auto'])],
     compile_and_run,
     [''])

test('T12962', [test_opts_dot_prof, grep_prof("T12962.hs")], compile_and_run, [''])

test('T14257', [], makefile_test, ['T14257'])

test('T15897',
     [extra_ways(['profasm']), only_ways(['profasm']),
      run_timeout_multiplier(2),
      fragile(15467)],
     makefile_test, ['T15897'])

test('T17572', [when(arch('wasm32'), run_timeout_multiplier(5))], compile_and_run, [''])

test('TraverseHeapTest', [only_ways(['prof'])], compile_and_run, ['-debug'])

# Check if -fno-prof-manual results in the manual cost center being ignored.
test('ignore_scc', [grep_prof("ignore_scc.hs"), test_opts_dot_prof], compile_and_run,
     ['-fno-prof-manual'])

test('T21446', [], makefile_test, ['T21446'])


test('scc-prof-overloaded001',
     [grep_prof("scc-prof-overloaded001.hs"), test_opts_dot_prof],
     compile_and_run,
     ['-fno-prof-auto -fno-full-laziness -fprof-late-overloaded'] # See Note [consistent stacks]
)

test('scc-prof-overloaded002',
     [grep_prof("scc-prof-overloaded002.hs"), test_opts_dot_prof],
     compile_and_run,
     ['-fno-prof-auto -fno-full-laziness -fprof-late-overloaded'] # See Note [consistent stacks]
)

test('scc-prof-overloaded-calls001',
     [grep_prof("scc-prof-overloaded-calls001.hs"), test_opts_dot_prof],
     compile_and_run,
     # Need optimizations to get rid of unwanted overloaded calls
     ['-O -fno-prof-auto -fno-full-laziness -fprof-late-overloaded-calls'] # See Note [consistent stacks]
)

test('scc-prof-overloaded-calls002',
     [grep_prof("scc-prof-overloaded-calls002.hs"), test_opts_dot_prof],
     compile_and_run,
     # Need optimizations to get rid of unwanted overloaded calls
     ['-O -fno-prof-auto -fprof-late-overloaded-calls']
)
