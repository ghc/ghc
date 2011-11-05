setTestOpts(only_compiler_types(['ghc']))

test('arrowfail001', normal, compile_fail, [''])
test('arrowfail002', normal, compile_fail, [''])
test('arrowfail003', normal, compile_fail, [''])
test('arrowfail004', normal, compile_fail, [''])
test('T2111', normal, compile_fail, [''])
test('T5380', normal, compile_fail, [''])
