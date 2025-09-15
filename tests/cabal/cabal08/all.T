test('cabal08',
     [extra_files(['Main.hs', 'Setup.hs', 'p1/', 'p2/']),
     js_broken(22349)],
     makefile_test,
     [])
