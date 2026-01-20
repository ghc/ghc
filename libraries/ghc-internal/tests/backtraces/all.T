exc_opts = [ when(have_profiling(), extra_ways(['prof']))
               , when(js_arch(), skip)
	       , exit_code(1)]

test('T14532a', [], compile_and_run, [''])
test('T14532b', [], compile_and_run, [''])
test('T26507', exc_opts, compile_and_run, [''])

# Stack traces shouldn't expose implementation details
test('T15395a', exc_opts, compile_and_run, [''])
test('T15395b', exc_opts, compile_and_run, [''])
test('T15395c', exc_opts, compile_and_run, [''])
test('T15395d', exc_opts, compile_and_run, [''])
test('T15395e', exc_opts, compile_and_run, [''])
