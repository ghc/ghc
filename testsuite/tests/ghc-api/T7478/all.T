test('T7478', [extra_files(['A.hs', 'B.hs', 'C.hs']),
               unless(have_dynamic(), skip)],
     makefile_test, ['T7478'])
