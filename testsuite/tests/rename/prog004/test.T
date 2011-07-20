test('rename.prog004',
     extra_clean(['A.hi', 'A.o', 'B.hi', 'B.o', 'C.hi', 'C.o']),
     multimod_compile,
     ['C', '-v0'])
