# Optimised only, we're testing the strictness analyser here
setTestOpts( only_ways(['opt']) )

test('strun001', normal, compile_and_run, [''])
test('strun002', exit_code(1), compile_and_run, [''])

