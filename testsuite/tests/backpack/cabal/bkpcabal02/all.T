test('bkpcabal02',
     [extra_files(['p', 'q', 'bkpcabal02.cabal', 'Setup.hs']),
      normalise_version('bkpcabal01')],
     makefile_test,
     [])
