setTestOpts(extra_ways(['sanity']))

test('compact_simple', [javascript_failures, normal], compile_and_run, [''])
test('compact_loop', [javascript_failures, normal], compile_and_run, [''])
test('compact_append', [javascript_failures, normal], compile_and_run, [''])
test('compact_autoexpand', [javascript_failures, normal], compile_and_run, [''])
test('compact_small_array', [javascript_failures, reqlib('primitive')], compile_and_run, [''])
test('compact_simple_array', [javascript_failures, normal], compile_and_run, [''])
test('compact_huge_array', [javascript_failures, normal], compile_and_run, [''])
test('compact_serialize', [javascript_failures, normal], compile_and_run, [''])
test('compact_largemap', [javascript_failures, normal], compile_and_run, [''])
test('compact_threads', [javascript_failures, extra_run_opts('1000') ], compile_and_run, [''])
test('compact_cycle', [javascript_failures, extra_run_opts('+RTS -K1m')], compile_and_run, [''])
test('compact_function', [javascript_failures, exit_code(1)], compile_and_run, [''])
test('compact_mutable', [javascript_failures, exit_code(1)], compile_and_run, [''])
test('compact_pinned', [javascript_failures, exit_code(1)], compile_and_run, [''])
test('compact_gc', [javascript_failures, fragile_for(17253, ['ghci']), ignore_stdout], compile_and_run, [''])
# this test computes closure sizes and those are affected
# by the ghci and prof ways, because of BCOs and profiling headers.
test('compact_share', [javascript_failures, omit_ways(['ghci', 'profasm', 'profthreaded'])],
		      compile_and_run, [''])
test('compact_bench', [ javascript_failures, ignore_stdout, extra_run_opts('100') ],
                       compile_and_run, [''])
test('T17044', [javascript_failures, normal], compile_and_run, [''])
# N.B. Sanity check times out due to large list.
test('T16992', [when(wordsize(32), skip), # Resource limit exceeded on 32-bit
                high_memory_usage,
                run_timeout_multiplier(5),
                omit_ways(['sanity'])], compile_and_run, [''])
