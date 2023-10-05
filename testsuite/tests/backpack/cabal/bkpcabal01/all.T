test('bkpcabal01',
     [extra_files(['p', 'q', 'impl', 'bkpcabal01.cabal', 'Setup.hs', 'Main.hs']),
      js_broken(22349),
      run_timeout_multiplier(2)],
     makefile_test,
     [])
