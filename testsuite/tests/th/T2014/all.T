test('T2014', [extra_files(['A.hs', 'A.hs-boot', 'B.hs', 'C.hs']),
               req_th],
     makefile_test, ['T2014'])
