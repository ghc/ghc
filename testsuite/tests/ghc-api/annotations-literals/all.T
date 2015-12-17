test('literals', extra_clean(['LiteralsTest.o', 'LiteralsTest.hi']),
                 run_command, ['$MAKE -s --no-print-directory literals'])
test('parsed',   extra_clean(['LiteralsTest2.o', 'LiteralsTest2.hi']),
                 run_command, ['$MAKE -s --no-print-directory parsed'])
