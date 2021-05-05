test('recomp-boot2', [extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs', 'C.hs-boot', 'Top.hs', 'M.hs']),
                   when(fast(), skip)],
     makefile_test, [])
