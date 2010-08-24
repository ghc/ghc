setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['optc', 'hpc', 'ghci', 'threaded1', 'threaded2']))

test('T2985', normal, compile_and_run, [''])
test('T4235', normal, compile_and_run, [''])

test('GMapAssoc', normal, compile_and_run, ['-package containers'])
test('GMapTop',   normal, compile_and_run, ['-package containers'])
