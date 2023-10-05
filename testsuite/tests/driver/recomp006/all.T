test('recomp006', [extra_files(['A.hs', 'B1.hs', 'B2.hs']),
                   when(fast(), skip)],
     makefile_test, [])
