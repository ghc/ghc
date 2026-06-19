test('T25924',
     [exit_code(1), ignore_stderr, extra_files(['Main.hs', 'B.hs'])],
     multimod_compile_and_run,
     ['Main', '-O'])
