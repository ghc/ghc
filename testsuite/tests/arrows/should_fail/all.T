test('arrowfail001',
     normal,
     compile_fail,
     [''])
 # arrowfail001 got an ASSERT error in the stage1 compiler
 # because we simply are not typechecking arrow commands
 # correctly.  See #5267, #5609, #5605
 # The fix is patch 'Fix the scope-nesting for arrows' Dec 2014

test('arrowfail002', normal, compile_fail, [''])
test('arrowfail003', normal, compile_fail, [''])
test('arrowfail004', normal, compile_fail, [''])
test('T2111', normal, compile_fail, [''])
test('T5380', normal, compile_fail, [''])
