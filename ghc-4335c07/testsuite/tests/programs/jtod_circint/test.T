
test('jtod_circint', [extra_files(['Bit.hs', 'LogFun.hs', 'Main.hs', 'Signal.hs']),
                      when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
