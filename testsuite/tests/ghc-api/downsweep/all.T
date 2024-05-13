setTestOpts(when(arch('wasm32'), run_timeout_multiplier(5)))

test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , ignore_stderr
     , js_fragile(24259)
     ],
     compile_and_run,
     ['-package ghc -package exceptions'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     , when(opsys('mingw32'), expect_broken(16772))
     ],
     compile_and_run,
     ['-package ghc'])
