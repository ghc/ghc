setTestOpts(extra_ways(['sanity', 'compacting_gc']))

test('compact_simple', normal, compile_and_run, [''])
test('compact_loop', normal, compile_and_run, [''])
test('compact_append', normal, compile_and_run, [''])
test('compact_autoexpand', normal, compile_and_run, [''])
test('compact_small_array', [reqlib('primitive')], compile_and_run, [''])
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
test('compact_share', omit_ways(['ghci', 'profasm', 'profthreaded']),
		      compile_and_run, [''])
test('compact_bench', [ ignore_stdout, extra_run_opts('100') ],
                       compile_and_run, [''])
test('T17044', normal, compile_and_run, [''])
# N.B. Sanity check times out due to large list.
test('T16992', [when(wordsize(32), skip), # Resource limit exceeded on 32-bit
                high_memory_usage,
                run_timeout_multiplier(5),
                omit_ways(['sanity'])], compile_and_run, [''])
