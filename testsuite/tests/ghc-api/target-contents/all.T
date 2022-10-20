test('TargetContents',
     [ extra_run_opts('"' + config.libdir + '"')
     , js_skip
     ]
     , compile_and_run,
     ['-package ghc -package exceptions'])
