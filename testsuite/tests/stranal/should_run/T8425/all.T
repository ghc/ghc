# Optimised only, we're testing the strictness analyser here
setTestOpts( only_ways(['optasm']) )

test('T8425', normal, multimod_compile_and_run, ['Main','-O2'])

