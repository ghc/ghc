test('T16608_1', [extra_files(['MyInteger.hs'])], run_command,
     ['$MAKE -s --no-print-directory T16608_1'])
test('T16608_2', [extra_files(['MyInteger.hs'])], run_command,
     ['$MAKE -s --no-print-directory T16608_2'])
