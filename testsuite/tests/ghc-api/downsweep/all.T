test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , ignore_stderr
     , js_broken(22349)
     ],
     compile_and_run,
     ['-package ghc -package exceptions'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     , when(opsys('mingw32'), expect_broken(16772))
     ],
     compile_and_run,
     ['-package ghc'])
