# Run this always as we compile the test with -O0 and -O1 and check that the
# output is correct and the same in both cases.
test('T16197', normal, makefile_test, [])

# Run the rest only in optasm way (which implies -O), we're testing the
# strictness analyser here
setTestOpts( only_ways(['optasm']) )

test('strun001', normal, compile_and_run, [''])
test('strun002', exit_code(1), compile_and_run, [''])
test('strun003', normal, compile_and_run, [''])
test('strun004', normal, compile_and_run, [''])
test('T2756b', normal, compile_and_run, [''])
test('T7649', normal, compile_and_run, [''])
test('T9254', normal, compile_and_run, [''])
test('T10148', normal, compile_and_run, [''])
test('T10218', normal, compile_and_run, [''])
test('T11076', normal, multimod_compile_and_run, ['T11076.hs', 'T11076_prim.cmm'])
test('T11555a', normal, compile_and_run, [''])
test('T12368', exit_code(1), compile_and_run, [''])
test('T12368a', exit_code(1), compile_and_run, [''])
test('T13380', exit_code(1), compile_and_run, [''])
test('T13380d', exit_code(1), compile_and_run, [''])
test('T14171', [expect_broken(14171), exit_code(1)], compile_and_run, [''])
test('T14290', normal, compile_and_run, [''])
test('T14285', normal, multimod_compile_and_run, ['T14285', ''])
test('T17676', normal, compile_and_run, [''])
