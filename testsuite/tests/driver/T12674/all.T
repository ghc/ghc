test('T12674', [extra_files(['-T12674.hs', '-T12674c.c'])], makefile_test, [])
test('T12674w', [extra_files(['-T12674.hs', '-T12674c.c']),
                unless(opsys('mingw32'), skip)],
                makefile_test, [])
