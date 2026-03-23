test('T27013a',
     [extra_files(['src/', 'composition.cabal', 'Setup.hs']),
      js_broken(22349)],
     makefile_test,
     [])
