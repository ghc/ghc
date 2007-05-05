setTestOpts(only_compiler_types(['ghc']))

test('arrowrun001', expect_broken_for(1333, ['ghci']), compile_and_run, [''])
test('arrowrun002', compose(expect_broken_for(1333, ['ghci']), skip_if_fast), compile_and_run, [''])
test('arrowrun003', expect_broken_for(1333, ['ghci']), compile_and_run, [''])
test('arrowrun004', compose(expect_broken(1333), skip_if_fast), compile_and_run, [''])
