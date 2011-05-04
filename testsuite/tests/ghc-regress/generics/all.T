setTestOpts(only_compiler_types(['ghc']))

test('CanDoRep0', normal, compile, [''])

test('ShouldFail0',  normal, compile_fail, [''])
test('CannotDoRep0', normal, compile_fail, [''])
test('CannotDoRep1', normal, compile_fail, [''])
test('CannotDoRep2', normal, compile_fail, [''])
