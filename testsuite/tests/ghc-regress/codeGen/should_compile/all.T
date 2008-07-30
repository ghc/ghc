test('cg001', only_compiler_types(['ghc']), compile, [''])
test('cg002', normal, compile, [''])
test('cg003', only_ways(['optc','optasm']), compile, ['-fvia-C'])
test('cg004', normal, compile, [''])
test('cg005', only_ways(['optc','optasm']), compile, [''])
test('cg006', normal, compile, [''])

test('1916', normal, compile, [''])
test('2388', normal, compile, [''])
