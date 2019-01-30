test('T2014', [extra_files(['A.hs', 'A.hs-boot', 'B.hs', 'C.hs']),
               req_interp], run_command,
     ['$MAKE -s --no-print-directory T2014'])
