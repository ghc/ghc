
# Missing:
# test('cc002', normal, compile_fail, [''])

test('ccfail001', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail002', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail003', only_compiler_types(['ghc']), compile_fail, [''])
test('T3066', only_compiler_types(['ghc']), compile_fail, [''])
