setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm']))

test('Simple1', normal, compile, [''])
test('Simple2', expect_broken(1053), compile, [''])
test('Simple3', normal, compile, [''])
test('Simple4', normal, compile, [''])
test('Simple5', normal, compile, [''])
test('Simple6', normal, compile, [''])

test('ind1', normal, compile, [''])
test('ind2', normal, multimod_compile, ['ind2', '-v0'])
test('impexp', normal, multimod_compile, 
     ['Imp', '-Wnot -no-hs-main -no-link'])
clean(['Ind2_help.hi', 'Ind2_help.o', 'Exp.hi', 'Imp.o'])
