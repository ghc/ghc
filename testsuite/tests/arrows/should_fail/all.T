setTestOpts(only_compiler_types(['ghc']))

test('arrowfail001',
     when(compiler_debugged(), expect_broken(5267)),
     compile_fail,
     [''])
 # arrowfail001 gets an ASSERT error in the stage1 compiler
 # because we simply are not typechecking arrow commands
 # correcly.  See Trac #5267, #5609, #5605

test('arrowfail002', normal, compile_fail, [''])
test('arrowfail003', normal, compile_fail, [''])
test('arrowfail004', normal, compile_fail, [''])
test('T2111', normal, compile_fail, [''])
test('T5380', normal, compile_fail, [''])
