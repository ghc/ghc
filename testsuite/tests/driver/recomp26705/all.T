test('recomp26705', [extra_files(['M2A.hs', 'M.hs', 'M2B.hs']),
                   when(fast(), skip), ignore_stdout],
     makefile_test, [])
