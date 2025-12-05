test('custom-external-interpreter-commands',
     [ extra_files(['Main.hs'])
     , windows_skip
     , when(config.cross, skip)
     , req_process
     , req_interp
     , omit_ways(prof_ways)
     ],
     multimod_compile_and_run,
     ['Main.hs', '-package ghci'])
