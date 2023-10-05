test('bkpcabal03',
     [extra_files(['asig1', 'asig2', 'bkpcabal03.cabal.in1', 'bkpcabal03.cabal.in2', 'Setup.hs', 'Mod.hs']),
      js_fragile(24259)],
     makefile_test,
     [])
