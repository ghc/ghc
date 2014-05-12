setTestOpts(only_compiler_types(['ghc']))

# setTestOpts(only_ways(['normal']));
# Not only-normal: want optimisation too, to check coercion optimiser

# In fast mode, we omit all the compile_and_run tests except a couple

test('gadt1', normal, compile, [''])
test('gadt2', when(fast(), skip), compile_and_run, [''])
test('gadt3', normal, compile, [''])
test('gadt4', when(fast(), skip), compile_and_run, [''])
test('gadt5', when(fast(), skip), compile_and_run, [''])
test('gadt6', normal, compile, [''])
test('gadt7', normal, compile_fail, [''])
test('gadt8', normal, compile, [''])
test('gadt9', normal, compile, [''])
test('gadt10', normal, compile_fail, [''])
test('gadt11', normal, compile_fail, [''])
test('gadt13', normal, compile_fail, [''])
test('gadt14', normal, compile, [''])
test('gadt15', normal, compile, [''])
test('gadt16', normal, compile, [''])

test('gadt17',
     extra_clean(['Gadt17_help.hi', 'Gadt17_help.o']),
     run_command,
     ['$MAKE -s --no-print-directory gadt17'])

test('gadt18', normal, compile, [''])
test('gadt19', normal, compile, [''])
test('gadt20', normal, compile, [''])
test('gadt21', normal, compile_fail, [''])
test('gadt22', normal, compile, [''])

test('gadt23',
     extra_clean(['Gadt23_AST.hi', 'Gadt23_AST.o']),
     run_command,
     ['$MAKE -s --no-print-directory gadt23'])

test('gadt24', normal, compile, [''])

test('red-black', normal, compile, [''])
test('type-rep', [ when(fast(), skip), when(compiler_debugged(),expect_broken_for(8569, ['hpc','optasm','threaded2','dyn','optllvm'])) ] , compile_and_run, [''])
test('equal', normal, compile, [''])
test('nbe', normal, compile, [''])
test('while', normal, compile_and_run, [''])
test('rw', normal, compile_fail, [''])
test('lazypat', normal, compile_fail, [''])
test('lazypatok', expect_fail, compile, [''])
test('tc', normal, compile_and_run, [''])
test('arrow', normal, compile, [''])
test('tdpe', normal, compile, [''])
test('Nilsson', when(fast(), skip), compile, [''])

if config.fast:
    test('records', normal, compile, [''])
else:
    test('records', normal, compile_and_run, [''])
test('ubx-records', when(fast(), skip), compile_and_run, [''])
test('records-fail1', normal, compile_fail, [''])

test('doaitse', normal, compile, [''])
test('josef', normal, compile, [''])

# Interaction of fundeps with GADTs doesn't work well 
test('gadt-fd', expect_broken(345), compile, [''])

test('karl1', normal, compile, [''])
test('karl2', normal, compile, [''])
test('data1', normal, compile, [''])
test('data2', normal, compile, [''])

test('termination', normal, compile, [''])
test('set', normal, compile, [''])
test('scoped', normal, compile, [''])
test('gadt-escape1', normal, compile_fail, [''])

# New ones from Dimitrios

# test('gadt-dim1', normal, compile, [''])
# test('gadt-dim2', normal, compile_fail, [''])
# test('gadt-dim3', normal, compile_fail, [''])
# test('gadt-dim4', normal, compile, [''])
# test('gadt-dim5', normal, compile, [''])
# test('gadt-dim6', normal, compile, [''])
# test('gadt-dim7', normal, compile, [''])
# test('gadt-dim8', normal, compile, [''])
# test('Arith', normal, compile, [''])

test('Session', normal, compile_and_run, [''])
test('CasePrune', normal, compile_fail, [''])

test('T1999', normal, compile, [''])
test('T1999a', normal, compile, [''])

test('T2587', normal, compile, [''])
test('T2040', normal, compile, [''])
test('T2151', normal, compile, [''])
test('T3013', normal, compile, [''])
test('T3163', normal, compile_fail, [''])
test('gadt25', normal, compile, [''])
test('T3651', normal, compile_fail, [''])
test('T3638', normal, compile, [''])

test('gadtSyntax001', when(compiler_lt('ghc', '7.1'), expect_fail), compile, [''])
test('gadtSyntaxFail001', when(compiler_lt('ghc', '7.1'), expect_fail), compile_fail, [''])
test('gadtSyntaxFail002', when(compiler_lt('ghc', '7.1'), expect_fail), compile_fail, [''])
test('gadtSyntaxFail003', when(compiler_lt('ghc', '7.1'), expect_fail), compile_fail, [''])
test('T3169', normal, compile_fail, [''])
test('T5424',
     extra_clean(['T5424a.hi', 'T5424a.o']),
     multimod_compile,
     ['T5424', '-v0 -O0'])

test('FloatEq', normal, compile, [''])
test('T7205', normal, compile, [''])
test('T7293', normal, compile_fail, [''])
test('T7294', normal, compile, [''])
test('T7321',
     extra_clean(['T7321a.hi', 'T7321a.o']),
     run_command,
     ['$MAKE -s --no-print-directory T7321'])
test('T7974', normal, compile, [''])
test('T7558', normal, compile_fail, [''])
test('T9096', normal, compile, [''])
