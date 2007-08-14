setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm', 'hpc']))

test('Simple1', normal, compile, [''])
test('Simple2', normal, compile, [''])
test('Simple3', normal, compile, [''])
test('Simple4', normal, compile, [''])
test('Simple5', normal, compile, [''])
test('Simple6', normal, compile, [''])
test('Simple7', normal, compile, [''])

test('ind1', normal, compile, [''])
test('ind2', normal, multimod_compile, ['ind2', '-v0'])
test('impexp', normal, multimod_compile, 
     ['Imp', '-Wnot -no-hs-main -no-link'])
clean(['Ind2_help.hi', 'Ind2_help.o', 'Exp.hi', 'Imp.o'])

test('ATLoop', normal, multimod_compile, ['ATLoop','-v0'])
clean(['ATLoop_Help.o','ATLoop_Help.hi'])

test('Deriving', normal, compile, [''])
test('DerivingNewType', expect_fail, compile, [''])
test('Records', normal, compile, [''])

# The point about this test is that it compiles NewTyCo1 and NewTyCo2
# *separately*
#
test('NewTyCo', ignore_output, run_command, ['$MAKE NewTyCo'])
clean(['NewTyCo1.o', 'NewTyCo1.hi', 'NewTyCo2.o', 'NewTyCo2.hi'])

test('Infix', expect_fail, compile, [''])

test('GADT1', expect_fail, compile, [''])
test('GADT2', expect_fail, compile, [''])
test('GADT3', expect_fail, compile, [''])

test('Class1', expect_fail, compile, [''])
test('Class2', normal, compile, [''])

