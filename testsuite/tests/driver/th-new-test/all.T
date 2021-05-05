test('th-new-test', [extra_files(['A.hs', 'B.hs', 'C.hs', 'D.hs', 'B1.hs', 'D1.hs', 'Main.hs']),
                   when(fast(), skip)
                   , normalise_slashes],
     makefile_test, [])
