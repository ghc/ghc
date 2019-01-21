test('recomp001', [extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs']),
                   when(fast(), skip)],
     makefile_test, [])
