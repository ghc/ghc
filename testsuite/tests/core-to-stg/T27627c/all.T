test('T27627c',
     [extra_files(['Main.hs', 'Caller.hs', 'Callee.hs'])],
     multimod_compile_and_run,
     ['Main', '-O'])
