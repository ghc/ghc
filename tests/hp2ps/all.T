test('T15904', [when(opsys('mingw32'), expect_broken(16388)), js_broken(22261)], makefile_test, [])
