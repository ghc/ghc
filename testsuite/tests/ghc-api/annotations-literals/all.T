test('literals', [extra_files(['LiteralsTest.hs'])], run_command, ['$MAKE -s --no-print-directory literals'])
test('parsed', [extra_files(['LiteralsTest2.hs'])], run_command, ['$MAKE -s --no-print-directory parsed'])
