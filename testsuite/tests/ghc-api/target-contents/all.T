test('TargetContents',
     [extra_run_opts('"' + config.libdir + '"')]
     , compile_and_run,
     ['-package ghc -package exceptions'])
