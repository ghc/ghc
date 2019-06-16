test('PartialDownsweep',
     [ extra_run_opts('"' + config.libdir + '"')
     , when(opsys('darwin'),
             use_specs({'stderr' : 'PartialDownsweep.darwin.stderr'})
           )
     , skip  # TODO remove before merging, ci issue
     ],
     compile_and_run,
     ['-package ghc'])

test('OldModLocation',
     [ extra_run_opts('"' + config.libdir + '"')
     ],
     compile_and_run,
     ['-package ghc'])
