test('T12674', [extra_files(['-T12674.hs', '-T12674c.c']),
                when(opsys('darwin') and arch('aarch64'), skip),
                req_c
                ],
                makefile_test, [])
test('T12674w', [extra_files(['-T12674.hs', '-T12674c.c']),
                unless(opsys('mingw32'), skip),
                req_c
                ],
                makefile_test, [])
