test('literals', [normalise_slashes, extra_files(['LiteralsTest.hs']), req_js_compactor], makefile_test, ['literals'])
test('parsed', [extra_files(['LiteralsTest2.hs']), req_js_compactor], makefile_test, ['parsed'])
