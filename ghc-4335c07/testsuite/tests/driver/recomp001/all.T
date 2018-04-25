test('recomp001', [extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs']),
                   when(fast(), skip)], run_command,
     ['$MAKE -s --no-print-directory recomp001'])
