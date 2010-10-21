setTestOpts(only_compiler_types(['ghc']))
# Keep optimised tests, so we test coercion optimisation
setTestOpts(omit_ways(['optasm', 'hpc']))

test('Simple1', normal, compile, [''])
test('Simple2', normal, compile, [''])
test('Simple3', normal, compile, [''])
test('Simple4', normal, compile, [''])
test('Simple5', normal, compile, [''])
test('Simple6', normal, compile, [''])
test('Simple7', normal, compile, [''])
test('Simple8', normal, compile, [''])
test('Simple9', normal, compile, [''])
test('Simple10', normal, compile, [''])
test('Simple11', normal, compile, [''])
test('Simple12', normal, compile, [''])
test('Simple13', normal, compile, [''])
test('Simple14', normal, compile_fail, [''])
test('Simple15', normal, compile, [''])
test('Simple16', normal, compile, [''])
test('Simple17', normal, compile, [''])
test('Simple18', normal, compile, [''])
test('Simple19', normal, compile, [''])
test('Simple20', expect_broken(4296), compile, [''])
test('Simple21', normal, compile, [''])
test('Simple22', normal, compile, [''])
test('Simple23', normal, compile, [''])
test('Simple24', normal, compile, [''])

test('RelaxedExamples', normal, compile, [''])
test('NonLinearLHS', normal, compile, [''])

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
     ['ATLoop.hs','-v0'])

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

test('GADT1', normal, compile, [''])
test('GADT2', normal, compile, [''])
test('GADT3', normal, compile, [''])
test('GADT4', normal, compile, [''])
test('GADT5', normal, compile, [''])
test('GADT6', normal, compile, [''])
test('GADT7', normal, compile, [''])
test('GADT8', normal, compile, [''])
test('GADT9', normal, compile, [''])
test('GADT10', normal, compile, [''])
test('GADT11', normal, compile, [''])
test('GADT12', normal, compile, [''])
test('GADT13', normal, compile, [''])
test('GADT14', normal, compile, [''])

test('Class1', normal, compile, [''])
test('Class2', normal, compile, [''])
test('Class3', normal, compile, [''])

test('Refl', normal, compile, [''])
test('Refl2', normal, compile, [''])

test('Rules1', normal, compile, [''])

test('Numerals', normal, compile, [''])

test('ColInference', normal, compile, [''])
test('ColInference2', normal, compile, [''])
test('ColInference3', normal, compile, [''])
test('ColInference4', normal, compile, [''])
test('ColInference5', normal, compile, [''])
test('ColInference6', normal, compile, [''])

test('ClassEqContext', normal, compile_fail, [''])
test('ClassEqContext2', normal, compile_fail, [''])
test('ClassEqContext3', normal, compile_fail, [''])

test('Col', normal, compile, [''])
test('Col2', normal, compile, [''])

test('ColGivenCheck', normal, compile, [''])
test('ColGivenCheck2', normal, compile, [''])

test('InstEqContext', expect_fail, compile, [''])
test('InstEqContext2', normal, compile, [''])
test('InstEqContext3', expect_fail, compile, [''])

test('InstContextNorm', normal, compile, [''])

test('HO', normal, compile_fail, [''])

test('GivenCheck', normal, compile, [''])
test('GivenCheckSwap', normal, compile, [''])
test('GivenCheckDecomp', normal, compile, [''])
test('GivenCheckTop', normal, compile, [''])

test('Gentle', normal, compile, [''])
test('T1981', normal, compile, [''])
test('T2238', expect_fail, compile, [''])
test('OversatDecomp', normal, compile, [''])

test('T2219', normal, compile, [''])
test('T2627', normal, compile, [''])
test('T2448', normal, compile, [''])
test('T2291', normal, compile, [''])
test('T2639', normal, compile, [''])
test('T2944', normal, compile, [''])
test('T3017', if_compiler_lt('ghc', '6.11', expect_fail), compile, ['-ddump-types'])
test('TF_GADT', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T2203b', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T2767', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T3208a', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T3208b', expect_fail, compile, [''])
test('T3418', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T3423', normal, compile, [''])
test('T2850', normal, compile, [''])
test('T3220', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T3590', normal, compile, [''])
test('CoTest3', if_compiler_lt('ghc', '6.13', expect_fail), compile, [''])
test('Roman1', if_compiler_lt('ghc', '6.13', expect_fail), compile, [''])
test('T4160', normal, compile, [''])
test('IndTypesPerf',
     extra_clean(['IndTypesPerf.o', 'IndTypesPerf.hi',
                  'IndTypesPerfMerge.o', 'IndTypesPerfMerge.hi']),
     run_command,
     ['$MAKE -s --no-print-directory IndTypesPerf'])

test('T4120', normal, compile_fail, [''])
test('T3787', reqlib('mtl'), compile, [''])
test('T3826', normal, compile, [''])
test('T4200', normal, compile, [''])
test('T3851', normal, compile, [''])
test('T4178', normal, compile, [''])
test('T3023', normal, compile, [''])
test('T4358', normal, compile, [''])
test('T4356', normal, compile, [''])
