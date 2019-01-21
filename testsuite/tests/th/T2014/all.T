test('T2014', [extra_files(['A.hs', 'A.hs-boot', 'B.hs', 'C.hs']),
               req_interp],
     makefile_test, ['T2014'])
