# Optimised only, we're testing the strictness analyser here
setTestOpts( only_ways(['optasm']) )

test('strun001', normal, compile_and_run, [''])
test('strun002', exit_code(1), compile_and_run, [''])
test('strun003', normal, compile_and_run, [''])
test('strun004', normal, compile_and_run, [''])
test('T2756b', normal, compile_and_run, [''])
test('T7649', normal, compile_and_run, [''])
