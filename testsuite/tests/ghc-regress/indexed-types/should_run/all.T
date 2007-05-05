setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['opt', 'optasm', 'ghci', 'threaded1', 'threaded2']))

test('GMapAssoc', expect_broken(1331), compile_and_run, [''])
test('GMapTop', expect_broken(1331), compile_and_run, [''])
