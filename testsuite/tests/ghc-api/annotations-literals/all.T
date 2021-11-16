test('literals', [javascript_failures, extra_files(['LiteralsTest.hs'])], makefile_test, ['literals'])
test('parsed', [javascript_failures, extra_files(['LiteralsTest2.hs'])], makefile_test, ['parsed'])
