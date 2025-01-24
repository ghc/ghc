setTestOpts(
  [extra_ways(['compacting_gc'] + (['sanity'] if debug_rts() else [])),
   js_skip # compact API not supported by the JS backend
  ])

test('compact_simple', normal, compile_and_run, [''])
test('compact_loop', normal, compile_and_run, [''])
test('compact_append', normal, compile_and_run, [''])
test('compact_autoexpand', normal, compile_and_run, [''])
test('compact_simple_array', normal, compile_and_run, [''])
test('compact_huge_array', normal, compile_and_run, [''])
test('compact_serialize', normal, compile_and_run, [''])
test('compact_largemap', normal, compile_and_run, [''])
test('compact_threads', [ extra_run_opts('1000') ], compile_and_run, [''])
test('compact_cycle', extra_run_opts('+RTS -K1m'), compile_and_run, [''])
test('compact_function', exit_code(1), compile_and_run, [''])
test('compact_mutable', exit_code(1), compile_and_run, [''])
test('compact_pinned', exit_code(1), compile_and_run, [''])
test('compact_gc', [fragile_for(17253, ['ghci']), ignore_stdout], compile_and_run, [''])
# this test computes closure sizes and those are affected
# by the ghci and prof ways, because of BCOs and profiling headers.
# Optimization levels slightly change what is/isn't shared so only run in normal mode
test('compact_share', only_ways(['normal']), compile_and_run, [''])
test('compact_bench', [ ignore_stdout, extra_run_opts('100') ],
                       compile_and_run, [''])
test('T17044', normal, compile_and_run, [''])
test('T18757', omit_ghci, compile_and_run, [''])
# N.B. Sanity check times out due to large list.
test('T16992', [when(wordsize(32), skip), # Resource limit exceeded on 32-bit
                high_memory_usage,
                # This test has extraordinary memory requirement,
                # skip unless testing slow speed
                unless(slow(), skip),
                run_timeout_multiplier(5),
                omit_ways(['sanity'])], compile_and_run, [''])
