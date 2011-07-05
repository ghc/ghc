setTestOpts(only_compiler_types(['ghc']))

test('GenCanDoRep0', normal, compile, [''])

test('GenShouldFail0',  normal, compile_fail, [''])
test('GenCannotDoRep0', normal, compile_fail, [''])
test('GenCannotDoRep1', normal, compile_fail, [''])
test('GenCannotDoRep2', normal, compile_fail, [''])
