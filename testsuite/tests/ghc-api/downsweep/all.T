test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , when(opsys("OSJavaScript"), skip)
     , ignore_stderr
     ],
     compile_and_run,
     ['-package ghc'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     , when(opsys("OSJavaScript"), skip)
     , when(opsys('mingw32'), expect_broken(16772))
     ],
     compile_and_run,
     ['-package ghc'])
