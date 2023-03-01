
test('jtod_circint', [extra_files(['Bit.hs', 'LogFun.hs', 'Main.hs', 'Signal.hs']), expect_broken_for(23272, ['ghci-opt']),
                      when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
