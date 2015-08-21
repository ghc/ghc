test('T10637', extra_clean(['T10637.o','T10637.hi', 'A.hi', 'A.o', 'A.hi-boot', 'A.o-boot']),
                      multimod_compile, ['T10637', '-v0 -fwarn-unused-imports'])
