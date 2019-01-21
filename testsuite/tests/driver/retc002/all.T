test('retc002', [extra_files(['Q.hs', 'W.hs', 'W.hs-boot']),
                 when(fast(), skip)],
     makefile_test, [])
