setTestOpts(only_compiler_types(['ghc']))

test('arrowrun001', normal, compile_and_run, [''])
test('arrowrun002', when(fast(), skip), compile_and_run, [''])
test('arrowrun003', normal, compile_and_run, [''])
test('arrowrun004', when(fast(), skip), compile_and_run, [''])
test('T3822', normal, compile_and_run, [''])

