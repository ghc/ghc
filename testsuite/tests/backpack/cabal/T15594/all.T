test('T15594',
     [extra_files(['Setup.hs', 'Stuff.hs', 'Sig.hsig', 'pkg.cabal', 'src'])],
     makefile_test,
     [])
