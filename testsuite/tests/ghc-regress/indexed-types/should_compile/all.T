setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['optc', 'optasm', 'hpc']))

test('Simple1', normal, compile, [''])
test('Simple2', normal, compile, [''])
test('Simple3', normal, compile, [''])
test('Simple4', normal, compile, [''])
test('Simple5', normal, compile, [''])
test('Simple6', normal, compile, [''])
test('Simple7', normal, compile, [''])
test('Simple8', expect_fail, compile, [''])
test('Simple9', normal, compile, [''])
test('Simple10', normal, compile, [''])
test('Simple11', normal, compile, [''])
test('Simple12', if_compiler_lt('ghc','6.9',expect_fail), compile, [''])
test('Simple13', normal, compile, [''])
test('Simple14', expect_fail, compile, [''])
test('Simple15', normal, compile, [''])
test('Simple16', normal, compile, [''])
test('Simple17', expect_fail, compile, [''])
test('Simple18', normal, compile, [''])
test('Simple19', normal, compile, [''])
test('Simple20', normal, compile, [''])
test('Simple21', normal, compile, [''])
test('Simple22', normal, compile, [''])
test('Simple23', normal, compile, [''])

test('ind1', normal, compile, [''])
test('ind2',
     extra_clean(['Ind2_help.hi', 'Ind2_help.o']),
     multimod_compile,
     ['ind2', '-v0'])
test('impexp',
     extra_clean(['Exp.hi', 'Exp.o', 'Imp.hi', 'Imp.o']),
     multimod_compile, 
     ['Imp', '-w -no-hs-main -c'])

test('ATLoop',
     extra_clean(['ATLoop_help.o','ATLoop_help.hi']),
     multimod_compile,
     ['ATLoop','-v0'])

test('Deriving', normal, compile, [''])
test('DerivingNewType', expect_fail, compile, [''])
test('Records', normal, compile, [''])

# The point about this test is that it compiles NewTyCo1 and NewTyCo2
# *separately*
#
test('NewTyCo',
     extra_clean(['NewTyCo1.o', 'NewTyCo1.hi', 'NewTyCo2.o', 'NewTyCo2.hi']),
     run_command,
     ['$MAKE -s --no-print-directory NewTyCo'])

test('Infix', normal, compile, [''])
test('Kind', normal, compile, [''])

test('GADT1', expect_fail, compile, [''])
test('GADT2', normal, compile, [''])
test('GADT3', if_compiler_ge('ghc', '6.9', expect_fail), compile, [''])
test('GADT4', if_compiler_lt('ghc', '6.9', expect_fail), compile, [''])
test('GADT5', if_compiler_lt('ghc', '6.9', expect_fail), compile, [''])
test('GADT6', if_compiler_lt('ghc', '6.9', expect_fail), compile, [''])
test('GADT7', expect_fail, compile, [''])
test('GADT8', normal, compile, [''])
test('GADT9', normal, compile, [''])
test('GADT10', expect_fail, compile, [''])
test('GADT11', normal, compile, [''])
test('GADT12', expect_fail, compile, [''])

test('Class1', normal, compile, [''])
test('Class2', normal, compile, [''])
test('Class3', expect_fail, compile, [''])

test('Refl', normal, compile, [''])
test('Refl2', normal, compile, [''])

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
test('InstEqContext2', if_compiler_lt('ghc', '6.9', expect_fail),
                       compile, [''])
test('InstEqContext3', normal, compile, [''])

test('InstContextNorm', if_compiler_lt('ghc', '6.9', expect_fail), compile, [''])

test('HO', normal, compile, [''])

test('GivenCheck', normal, compile, [''])
test('GivenCheckSwap', normal, compile, [''])
test('GivenCheckDecomp', normal, compile, [''])
test('GivenCheckTop', normal, compile, [''])

test('Gentle', normal, compile, [''])
test('T1981', normal, compile, [''])
test('T2238', if_compiler_lt('ghc', '6.9', expect_fail), compile, [''])
