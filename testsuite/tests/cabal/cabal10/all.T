test('cabal10',
     [extra_files(['Use.hs', 'Setup.hs', 'src/', 'internal-lib.cabal']),
     js_broken(22349)],
     makefile_test,
     [])
