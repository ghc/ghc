test('T27703',
     [extra_files(['Main.hs', 'Lib.hs'])],
     multimod_compile_and_run,
     ['Main', '-O'])
