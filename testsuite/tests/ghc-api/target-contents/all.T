test('TargetContents',
     [ extra_run_opts('"' + config.libdir + '"')
     , js_broken(22362)
     ]
     , compile_and_run,
     ['-package ghc -package exceptions'])
