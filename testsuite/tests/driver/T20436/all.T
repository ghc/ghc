test('T20436', [extra_files(['A.hs']), when(opsys('mingw32'), skip), unless(have_dynamic(), skip)], makefile_test, [])
