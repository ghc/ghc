setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm', 'hpc']))

test('Simple1', normal, compile, [''])
test('Simple2', normal, compile, [''])
test('Simple3', normal, compile, [''])
test('Simple4', normal, compile, [''])
test('Simple5', normal, compile, [''])
test('Simple6', normal, compile, [''])
test('Simple7', normal, compile, [''])
test('Simple8', expect_fail, compile, [''])
test('Simple9', normal, compile, [''])
test('Simple10', expect_fail, compile, [''])
test('Simple11', expect_fail, compile, [''])
test('Simple12', expect_fail_if_compiler_lt('ghc','6.9'), compile, [''])
test('Simple13', normal, compile, [''])
test('Simple14', expect_fail, compile, [''])
test('Simple15', expect_fail, compile, [''])
test('Simple16', normal, compile, [''])
test('Simple17', expect_fail, compile, [''])
test('Simple18', normal, compile, [''])
test('Simple19', normal, compile, [''])
test('Simple20', expect_fail, compile, [''])

test('ind1', normal, compile, [''])
test('ind2', expect_fail_ways_if_compiler_lt('ghc','6.9',['profc','profasm']),
             multimod_compile, ['ind2', '-v0'])
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

test('Infix', normal, compile, [''])
test('Kind', normal, compile, [''])

test('GADT1', expect_fail, compile, [''])
test('GADT2', normal, compile, [''])
test('GADT3', expect_broken_for(1738, ['normal','optc']), compile, [''])
test('GADT4', expect_fail, compile, [''])
test('GADT5', expect_fail, compile, [''])
test('GADT6', expect_fail, compile, [''])
test('GADT7', expect_fail, compile, [''])
test('GADT8', normal, compile, [''])
test('GADT9', expect_fail, compile, [''])
test('GADT10', expect_fail, compile, [''])

test('Class1', normal, compile, [''])
test('Class2', normal, compile, [''])
test('Class3', expect_fail, compile, [''])

test('Refl', normal, compile, [''])
test('Refl2', expect_fail, compile, [''])

test('Rules1', normal, compile, [''])

test('RankN1', expect_fail, compile, [''])

test('Numerals', normal, compile, [''])

test('ColInference', normal, compile, [''])
test('ColInference2', normal, compile, [''])
test('ColInference3', normal, compile, [''])
test('ColInference4', normal, compile, [''])
test('ColInference5', normal, compile, [''])
test('ColInference6', normal, compile, [''])

test('ClassEqContext', normal, compile, [''])
test('ClassEqContext2', normal, compile, [''])
test('ClassEqContext3', normal, compile, [''])

test('Col', normal, compile, [''])
test('Col2', normal, compile, [''])

test('ColGivenCheck', normal, compile, [''])
test('ColGivenCheck2', normal, compile, [''])

test('InstEqContext', normal, compile, [''])
test('InstEqContext2', expect_fail, compile, [''])
test('InstEqContext3', normal, compile, [''])

test('HO', normal, compile, [''])

test('GivenCheck', normal, compile, [''])
test('GivenCheckSwap', normal, compile, [''])
test('GivenCheckDecomp', normal, compile, [''])
test('GivenCheckTop', normal, compile, [''])
