test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , ignore_stderr
     , req_js_compactor
     ],
     compile_and_run,
     ['-package ghc -package exceptions'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     , js_skip
     , when(opsys('mingw32'), expect_broken(16772))
     ],
     compile_and_run,
     ['-package ghc'])
