test('rename.prog002',
     extra_clean(['Rn037Help.hi', 'Rn037Help.o']),
     multimod_compile_fail,
     ['rnfail037', '-v0'])
