setTestOpts(only_compiler_types(['ghc']))
setTestOpts(omit_ways(['optc', 'hpc', 'ghci', 'threaded1', 'threaded2']))

test('GMapAssoc', 
        expect_fail_ways_if_compiler_lt('ghc', '6.9',
                                        ['optasm','profc','profasm']),
        compile_and_run, ['-package containers'])

test('GMapTop', 
     expect_fail_ways_if_compiler_lt('ghc', '6.9',
                                     ['optasm','profc','profasm']),
     compile_and_run, ['-package containers'])
