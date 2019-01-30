test('T15970', [extra_files(['A1.hs', 'A2.hs', 'B.hs', 'C.hs'])],
               run_command, ['$MAKE -s --no-print-directory T15970'])
