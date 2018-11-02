test('T12674', [extra_files(['-T12674.hs', '-T12674c.c'])],
                multi_compile, ['./-T12674.hs', [('././-T12674c.c', '')], '-v0'])
test('T12674w', [extra_files(['-T12674.hs', '-T12674c.c']),
                unless(opsys('mingw32'), skip)],
                multi_compile, ['.\\\-T12674.hs', [('.\\\.\\\-T12674c.c', '')], '-v0'])
