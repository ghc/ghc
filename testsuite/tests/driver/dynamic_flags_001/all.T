test('dynamic_flags_001', [extra_files(['A.hs', 'B.hs', 'C.hs']),
                           when(fast(), skip)], run_command,
     ['$MAKE -s --no-print-directory dynamic_flags_001'])
