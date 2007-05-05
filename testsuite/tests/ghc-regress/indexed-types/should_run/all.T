setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm', 'ghci', 'threaded1', 'threaded2']))

test('GMapAssoc', normal, compile_and_run, [''])
test('GMapTop', normal, compile_and_run, [''])
