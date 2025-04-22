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

test('gadt17', [extra_files(['Gadt17_help.hs'])], makefile_test, ['gadt17'])

test('gadt18', normal, compile, [''])
test('gadt19', normal, compile, [''])
test('gadt20', normal, compile, [''])
test('gadt21', normal, compile_fail, [''])
test('gadt22', normal, compile, [''])

test('gadt23', [copy_files, extra_files(['Gadt23_AST.hs'])], makefile_test, ['gadt23'])

test('gadt24', normal, compile, [''])

test('red-black', normal, compile, [''])
test('type-rep', when(fast(), skip), compile_and_run, [''])
test('equal', normal, compile, [''])
test('nbe', normal, compile, [''])
test('while', normal, compile_and_run, [''])
test('rw', normal, compile_fail, [''])
test('lazypat', normal, compile_fail, [''])
# Not marked as expect_broken, because it's not clear whether this test should
# succeed or fail.
test('lazypatok', expect_fail, compile, [''])
test('tc', normal, compile_and_run, [''])
test('arrow', normal, compile, [''])
test('tdpe', normal, compile, [''])
test('Nilsson', when(fast(), skip), compile, [''])

if fast():
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
test('T3651', normal, compile, [''])
test('T3638', normal, compile, [''])

test('gadtSyntax001', normal, compile, [''])
test('gadtSyntax002', normal, compile, [''])
test('gadtSyntaxFail001', normal, compile_fail, [''])
test('gadtSyntaxFail002', normal, compile_fail, [''])
test('gadtSyntaxFail003', normal, compile_fail, [''])
test('T3169', normal, compile_fail, [''])
test('T5424', [], multimod_compile, ['T5424', '-v0 -O0'])

test('FloatEq', normal, compile, [''])
test('T7205', normal, compile, [''])
test('T7293', normal, compile_fail, ['-Werror'])
test('T7294', normal, compile, [''])
test('T7321', [], makefile_test, [])
test('T7974', normal, compile, [''])
test('T7558', normal, compile, [''])
test('T9380', normal, compile_and_run, [''])
test('T12087', normal, compile_fail, [''])
test('T12468', normal, compile_fail, [''])
test('T14320', normal, compile_fail, [''])
test('T14719', normal, compile_fail, ['-fdiagnostics-show-caret'])
test('T14808', normal, compile, [''])
test('T15009', normal, compile, [''])
test('T15558', normal, compile, [''])
test('T16427', normal, compile_fail, [''])
test('T18191', normal, compile_fail, [''])
test('T20278', normal, compile, [''])
test('SynDataRec', normal, compile, [''])
test('T20485', normal, compile, [''])
test('T20485a', normal, compile, [''])
test('T22235', normal, compile, [''])
test('T19847', normal, compile, [''])
test('T19847a', normalise_version('base'), compile, ['-ddump-types'])
test('T19847b', normal, compile, [''])
test('T23022', normal, compile, ['-dcore-lint'])
test('T23023', normal, compile_fail, ['-O -dcore-lint']) # todo: move this test?
test('T23298', normal, compile_fail, [''])
