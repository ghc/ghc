test('T14532a', [], compile_and_run, [''])
test('T14532b', [], compile_and_run, [''])
test('T26507', [ when(have_profiling(), extra_ways(['prof']))
               , when(js_arch(), skip)
               , when(ghc_with_ipe(), skip) # IPE builds include an IPE backtrace section on stderr.
               , exit_code(1)], compile_and_run, [''])

# Stack traces shouldn't expose implementation details
test('T15395', [ when(have_profiling(), extra_ways(['prof']))
               , when(js_arch(), skip)], compile_and_run, [''])
