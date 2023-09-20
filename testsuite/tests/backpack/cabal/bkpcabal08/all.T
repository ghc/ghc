test('bkpcabal08',
     [extra_files(['p', 'q', 'impl', 'bkpcabal08.cabal', 'Setup.hs', 'R.hs']),
      js_broken(22351), 
      fragile(23648),
      normalise_slashes,
      normalise_version('bkpcabal08')],
     makefile_test,
     [])
