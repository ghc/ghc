test('T18567',
     [ extra_files(['Setup.hs', 'sublib/', 'sublib-unused', 'src/', 'internal-lib.cabal'])
     , js_broken(22349)
     , normalise_version('internal-lib')
     ],
     makefile_test,
     [])
