# Test recompilation checking on signatures
test('bkpcabal04',
     [extra_files(['p', 'q', 'bkpcabal04.cabal.in1', 'bkpcabal04.cabal.in2', 'Setup.hs'])],
     makefile_test,
     [])
