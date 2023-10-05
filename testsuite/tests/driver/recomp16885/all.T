test('recomp16885', [extra_files(['M2A.hs', 'M.hs', 'M1.hs', 'M2B.hs', 'M3.hs']),
                   when(fast(), skip)],
     makefile_test, [])
