test('T12062', extra_clean(['T12062.o','T12062.hi', 'A.hi', 'A.o', 'A.hi-boot', 'A.o-boot', 'C.hi', 'C.o']),
                      multimod_compile_fail, ['T12062', '-v0 -j0'])
