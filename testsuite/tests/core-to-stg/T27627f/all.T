test('T27627f',
     [extra_files(['Main.hs', 'Inst.hs', 'Caller.hs', 'Callee.hs'])],
     multimod_compile_and_run,
     ['Main', '-O'])
