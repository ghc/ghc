setTestOpts(only_compiler_types(['ghc']))

test('arrowrun001', normal, compile_and_run, [''])
test('arrowrun002', skip_if_fast, compile_and_run, [''])
test('arrowrun003', normal, compile_and_run, [''])
test('arrowrun004', skip_if_fast, compile_and_run, [''])
