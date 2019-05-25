test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , when(opsys('darwin'), skip) # use_specs doesn't exist on this branch yet
     ],
     compile_and_run,
     ['-package ghc'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     ],
     compile_and_run,
     ['-package ghc'])
