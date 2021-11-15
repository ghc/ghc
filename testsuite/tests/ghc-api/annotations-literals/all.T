test('literals', [when(opsys("OSJavaScript"), skip), extra_files(['LiteralsTest.hs'])], makefile_test, ['literals'])
test('parsed', [when(opsys("OSJavaScript"), skip), extra_files(['LiteralsTest2.hs'])], makefile_test, ['parsed'])
