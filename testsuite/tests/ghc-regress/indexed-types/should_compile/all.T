setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm']))

test('ind1', normal, compile, [''])
test('ind2', normal, multimod_compile, ['ind2', '-v0'])
clean(['Ind2_help.hi', 'Ind2_help.o'])
