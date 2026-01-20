stack_annotation_backtrace_opts = [ when(have_profiling(), extra_ways(['prof'])) , when(js_arch(), skip) , exit_code(1) ]

test('T26806a', stack_annotation_backtrace_opts, compile_and_run, [''])
test('T26806b', stack_annotation_backtrace_opts, compile_and_run, [''])
test('T26806c', stack_annotation_backtrace_opts, compile_and_run, [''])
