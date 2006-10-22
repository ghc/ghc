setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm']))

test('ind1', normal, compile, [''])
test('ind2', normal, multimod_compile, ['ind2', '-v0'])
test('impexp', normal, multimod_compile, 
     ['Imp', '-Wnot -no-hs-main -no-link'])
clean(['Ind2_help.hi', 'Ind2_help.o', 'Exp.hi', 'Imp.o'])
