test('boot5688', [extra_files(['A.hs', 'B.hs', 'B.hs-boot', 'C.hs', 'D.hs', 'E.hs']),
                   when(fast(), skip)],
     makefile_test, [])
