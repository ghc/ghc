test('bug1465', [extra_files(['B1.hs', 'B2.hs', 'C.hs', 'v1/', 'v2/'])], run_command, ['$MAKE -s --no-print-directory bug1465'])
