test('recomp-boot', [extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs', 'C.hs-boot']),
                   when(fast(), skip)],
     makefile_test, [])
