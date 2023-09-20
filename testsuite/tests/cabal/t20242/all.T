test('T20242',
     [extra_files(['Setup.hs', 'BootNoHeader.cabal','Foo.hs', 'Foo.hs-boot', 'Main.hs'])],
     makefile_test,
     [])
