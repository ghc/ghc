test('rename.prog003',
     extra_clean(['A.hi', 'A.o']),
     multimod_compile_fail,
     ['B', '-v0'])
