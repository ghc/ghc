
# These tests all currently fail, as GADTs aren't
# supported in arrow notation.
# It would be nice to add proper support, but for the
# time being we conservatively reject these programs,
# to avoid Core Lint errors.

test('ArrowDict', normal, compile_fail, [''])
test('ArrowGADTKappa', normal, compile_fail, [''])
test('ArrowExistential', normal, compile_fail, [''])
 # ArrowExistential got an ASSERT error in the stage1 compiler
 # because we simply are not typechecking arrow commands
 # correctly.  See #5267, #5609, #5605
 #
 # Dec 2014: The fix is patch 'Fix the scope-nesting for arrows'
 # Oct 2021 update: we currently reject all GADT pattern matches
 # in arrows, as the implementation just isn't up to it currently.

# Using let-bindings is a workaround for matching on
# GADTs within arrow notation.
test('ArrowLet1', normal, compile, [''])
test('ArrowLet2', normal, compile, [''])
test('ArrowLet3', normal, compile, [''])

test('ArrowPatSyn1', normal, compile_fail, [''])
test('ArrowPatSyn2', normal, compile_fail, [''])

test('T5777', normal, compile_fail, [''])
test('T9985', normal, compile_fail, [''])
test('T17423', normal, compile_fail, [''])
test('T20469', normal, compile_fail, [''])
test('T20470', normal, compile_fail, [''])
