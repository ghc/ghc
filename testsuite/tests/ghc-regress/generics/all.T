setTestOpts(only_compiler_types(['ghc']))

test('canDoRep0', normal, compile, [''])

test('cannotDoRep0', normal, compile_fail, [''])
test('cannotDoRep1', normal, compile_fail, [''])
test('cannotDoRep2', normal, compile_fail, [''])
