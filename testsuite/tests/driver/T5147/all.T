test('T5147', [extra_files(['A.hs', 'B1.hs', 'B2.hs'])], run_command, ['$MAKE -s --no-print-directory T5147'])
