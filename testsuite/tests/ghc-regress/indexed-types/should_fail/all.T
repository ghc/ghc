setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm']))

test('Over', normal, multimod_compile, ['OverD', ''])
clean(['OverA.hi', 'OverA.o', 'OverB.hi', 'OverB.o', 'OverC.hi', 'OverC.o'])
