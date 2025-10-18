test('T14532a', [], compile_and_run, [''])
test('T14532b', [], compile_and_run, [''])
test('T26507', [ when(have_profiling(), extra_ways(['prof']))
               , when(js_arch(), skip)
	       , exit_code(1)], compile_and_run, [''])

