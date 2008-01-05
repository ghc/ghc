test('read001', normal, compile_and_run, [''])
test('read002', normal, compile_and_run, [''])
test('read003', normal, compile_and_run, [''])
test('read004', compose(expect_broken_for(1257, ['ghci']),
                        if_compiler_lt('ghc', '6.9', skip))
              , compile_and_run, [''])
