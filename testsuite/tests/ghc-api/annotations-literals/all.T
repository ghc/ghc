test('literals', [normalise_slashes, extra_files(['LiteralsTest.hs'])], makefile_test, ['literals'])
test('parsed', [extra_files(['LiteralsTest2.hs'])], makefile_test, ['parsed'])
