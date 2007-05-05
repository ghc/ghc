setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm', 'hpc']))

test('Simple1', expect_broken(1331), compile, [''])
test('Simple2', expect_broken(1331), compile, [''])
test('Simple3', expect_broken(1331), compile, [''])
test('Simple4', expect_broken(1331), compile, [''])
test('Simple5', expect_broken(1331), compile, [''])
test('Simple6', expect_broken(1331), compile, [''])

test('ind1', expect_broken(1331), compile, [''])
test('ind2', expect_broken(1331), multimod_compile, ['ind2', '-v0'])
test('impexp', expect_broken(1331), multimod_compile, 
     ['Imp', '-Wnot -no-hs-main -no-link'])
clean(['Ind2_help.hi', 'Ind2_help.o', 'Exp.hi', 'Imp.o'])

test('ATLoop', expect_broken(1331), multimod_compile, ['ATLoop','-v0'])
clean(['ATLoop_Help.o','ATLoop_Help.hi'])

test('Deriving', expect_fail, compile, [''])
test('Records', expect_broken(1331), compile, [''])
