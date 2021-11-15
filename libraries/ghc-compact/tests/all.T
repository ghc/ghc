setTestOpts(extra_ways(['sanity']))

test('compact_simple', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_loop', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_append', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_autoexpand', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_small_array', [when(opsys('OSJavaScript'), skip), reqlib('primitive')], compile_and_run, [''])
test('compact_simple_array', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_huge_array', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_serialize', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_largemap', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
test('compact_threads', [when(opsys('OSJavaScript'), skip), extra_run_opts('1000') ], compile_and_run, [''])
test('compact_cycle', [when(opsys('OSJavaScript'), skip), extra_run_opts('+RTS -K1m')], compile_and_run, [''])
test('compact_function', [when(opsys('OSJavaScript'), skip), exit_code(1)], compile_and_run, [''])
test('compact_mutable', [when(opsys('OSJavaScript'), skip), exit_code(1)], compile_and_run, [''])
test('compact_pinned', [when(opsys('OSJavaScript'), skip), exit_code(1)], compile_and_run, [''])
test('compact_gc', [when(opsys('OSJavaScript'), skip), fragile_for(17253, ['ghci']), ignore_stdout], compile_and_run, [''])
# this test computes closure sizes and those are affected
# by the ghci and prof ways, because of BCOs and profiling headers.
test('compact_share', [when(opsys('OSJavaScript'), skip), omit_ways(['ghci', 'profasm', 'profthreaded'])],
		      compile_and_run, [''])
test('compact_bench', [ when(opsys('OSJavaScript'), skip), ignore_stdout, extra_run_opts('100') ],
                       compile_and_run, [''])
test('T17044', [when(opsys('OSJavaScript'), skip), normal], compile_and_run, [''])
# N.B. Sanity check times out due to large list.
test('T16992', [when(wordsize(32), skip), # Resource limit exceeded on 32-bit
                high_memory_usage,
                run_timeout_multiplier(5),
                omit_ways(['sanity'])], compile_and_run, [''])
