test('rename.prog001',
     extra_clean(['Rn037Help.hi', 'Rn037Help.o', 'rn037.hi', 'rn037.o']),
     multimod_compile,
     ['rn037', '-v0'])
