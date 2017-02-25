test('T7478', [extra_files(['A.hs', 'B.hs', 'C.hs']),
               unless(have_dynamic(), skip)], run_command,
     ['$MAKE -s --no-print-directory T7478'])
