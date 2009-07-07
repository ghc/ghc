setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['optc', 'optasm']))

test('Simple1a', normal, compile_fail, [''])
test('Simple1b', normal, compile_fail, [''])
test('Simple2a', normal, compile_fail, [''])
test('Simple2b', normal, compile_fail, [''])
test('Simple3a', normal, compile_fail, [''])
test('Simple4', normal, compile_fail, [''])
test('Simple5a', normal, compile_fail, [''])
test('Simple5b', normal, compile_fail, [''])
test('Simple6', normal, compile_fail, [''])
test('Simple7', normal, compile_fail, [''])
test('Simple8', normal, compile_fail, [''])
test('Simple9', normal, compile_fail, [''])
test('Simple10', normal, compile_fail, [''])
test('Simple11a', normal, compile_fail, [''])
test('Simple11b', normal, compile_fail, [''])
test('Simple11c', normal, compile_fail, [''])
test('Simple11d', normal, compile_fail, [''])
test('Simple12', normal, compile_fail, [''])
test('Simple13', normal, compile_fail, [''])
test('Simple14', normal, compile_fail, [''])
test('Simple15', normal, compile_fail, [''])
test('Simple16', normal, compile_fail, [''])
test('TyFamArity1', normal, compile_fail, [''])
test('TyFamArity2', normal, compile_fail, [''])
test('TyFamUndec', normal, compile_fail, [''])

test('NotRelaxedExamples', normal, compile_fail, [''])
test('NonLinearSigErr', normal, compile, [''])

test('GADTwrong1', normal, compile_fail, [''])

test('Over',
     extra_clean(['OverA.hi', 'OverA.o',
                  'OverB.hi', 'OverB.o',
                  'OverC.hi', 'OverC.o']),
     multimod_compile_fail,
     ['OverD', '-no-hs-main -c -v0'])

test('SkolemOccursLoop', expect_fail, compile_fail, [''])

test('T2334', normal, compile_fail, [''])
test('T1900', normal, compile_fail, [''])
test('T2157', normal, compile_fail, [''])
test('T2203a', normal, compile_fail, [''])
test('T2627b', normal, compile_fail, [''])
test('T2693', normal, compile_fail, [''])
test('T2888', normal, compile, [''])
test('T3092', normal, compile_fail, [''])
test('NoMatchErr', normal, compile_fail, [''])
test('T2677', normal, compile_fail, [''])
