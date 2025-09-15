test('T16219',
     [ extra_files(['Setup.hs', 'backpack-issue.cabal', 'library-a', 'library-a-impl', 'library-b'])
     , when(opsys('mingw32'), fragile(17452))
     , js_broken(22349)
     ],
     makefile_test,
     [])
