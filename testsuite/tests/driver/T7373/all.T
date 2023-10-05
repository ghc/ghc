test('T7373', [extra_files(['D.hs', 'pkg/']),
               expect_broken(7373)],
     makefile_test, [])
