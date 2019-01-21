test('dynamic_flags_001', [extra_files(['A.hs', 'B.hs', 'C.hs']),
                           when(fast(), skip)],
     makefile_test, [])
