test('T15904', [when(opsys("OSJavaScript"), skip), when(opsys('mingw32'), expect_broken(16388))], makefile_test, [])
