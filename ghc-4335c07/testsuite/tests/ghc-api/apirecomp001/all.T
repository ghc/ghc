test('apirecomp001',
     extra_files(['A.hs', 'B.hs', 'myghc.hs']),
     run_command,
     ['$MAKE -s --no-print-directory apirecomp001'])
