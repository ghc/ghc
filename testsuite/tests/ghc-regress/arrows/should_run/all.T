setTestOpts(only_compiler_types(['ghc']))

test('arrowrun001', expect_broken_for(1333, ['ghci']), compile_and_run, [''])
test('arrowrun002', composes([expect_broken_for(1333, ['ghci']),
                              skip_if_fast,
                              if_compiler_lt('ghc', '6.9', skip)])
                  , compile_and_run, [''])
test('arrowrun003',
     compose(expect_broken_for(1333, ['ghci']),
             if_compiler_lt('ghc', '6.9', skip)),
     compile_and_run, [''])
test('arrowrun004', compose(skip_if_fast,
                            if_compiler_lt('ghc', '6.9', skip))
                  , compile_and_run, [''])
