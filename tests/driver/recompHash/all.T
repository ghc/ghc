test('recompHash', [extra_files(['A.hs', 'B.hs']),
                   when(fast(), skip)],
     makefile_test, [])
