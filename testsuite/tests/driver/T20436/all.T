test('T20436', [extra_files(['A.hs']), when(opsys('mingw32'), skip)], makefile_test, [])
