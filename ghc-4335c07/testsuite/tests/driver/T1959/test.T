test('T1959', [extra_files(['B.hs', 'C.hs', 'D.hs', 'E1.hs', 'E2.hs'])], run_command, ['$MAKE -s --no-print-directory dotest'])
