test('barton-mangler-bug',
     [extra_files(['Basic.hs', 'Expected.hs', 'Main.hs', 'Physical.hs', 'Plot.lhs', 'PlotExample.lhs', 'TypesettingTricks.hs']),
      when(fast(), skip),
      omit_ways(['debug']) # Fails for debug way due to annotation linting timeout
      ],
     multimod_compile_and_run, ['Main', ''])

