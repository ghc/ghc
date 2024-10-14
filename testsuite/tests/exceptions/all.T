test('T25052', normal, compile_and_run, [''])
test('T25300a', [when(opsys('mingw32'), skip) # this is broken on windows for some reason
                , extra_files(['T25300.hs']), normal], multimod_compile_and_run, ['T25300.hs', ''])
test('T25300b', [extra_files(['T25300.hs']), normal, expect_broken(25300)], multimod_compile_and_run, ['T25300.hs', '-DBRACKET'])
