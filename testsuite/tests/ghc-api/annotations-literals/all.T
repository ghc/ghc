test('literals', [normalise_slashes, extra_files(['LiteralsTest.hs']), js_broken(22352)], makefile_test, ['literals'])
test('parsed', [extra_files(['LiteralsTest2.hs']), js_broken(22352)], makefile_test, ['parsed'])
