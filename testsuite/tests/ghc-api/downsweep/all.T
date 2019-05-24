test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , exit_code(1)
     ],
     compile_and_run,
     ['-package ghc'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     ],
     compile_and_run,
     ['-package ghc'])
