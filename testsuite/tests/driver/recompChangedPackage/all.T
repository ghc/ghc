test('recompChangedPackage', [extra_files(['q', 'Main.hs', 'Setup.hs', 'PLib1.hs']),
                   when(fast(), skip)],
     makefile_test, [])
