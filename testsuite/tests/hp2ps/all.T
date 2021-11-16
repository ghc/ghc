test('T15904', [javascript_failures, when(opsys('mingw32'), expect_broken(16388))], makefile_test, [])
