# Optimised only, we're testing the strictness analyser here
setTestOpts( only_ways(['optasm']) )

test('T8425', extra_files(['Arr.hs', 'Base.hs', 'BuggyOpt.hs', 'Good.hs', 'M.hs', 'Main.hs']), multimod_compile_and_run, ['Main','-O2'])

