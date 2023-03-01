test('TargetContents',
     [ extra_run_opts('"' + config.libdir + '"')
     , js_broken(22362)
     , expect_broken_for(23272, ['ghci-opt'])
     , req_process
     ]
     , compile_and_run,
     ['-package ghc -package exceptions'])
