test('bkpcabal06',
     [extra_files(['bkpcabal06.cabal', 'Setup.hs', 'sig', 'impl']),
      js_broken(22349),
      when(opsys('mingw32'), skip)],
     makefile_test,
     [])
