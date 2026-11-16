test('interp-cancellation',
     [ extra_files(['Main.hs'])
     , windows_skip
     , when(config.cross, skip)
     , req_process
     , req_interp
     , omit_ways(prof_ways)
     ],
     multimod_compile_and_run,
     ['Main.hs', '-package ghc -package ghci'])

test('interp-concurrent',
     [ extra_run_opts(f'"{config.libdir}"')
     , windows_skip
     , when(config.cross, skip)
     , req_th
     , unless(config.have_ext_interp, skip)
     , only_ways(['normal'])
     ],
     compile_and_run,
     ['-package ghc -threaded'])
