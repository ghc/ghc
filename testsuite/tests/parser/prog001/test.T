test('parser.prog001',
     extra_clean(['Read006.hi', 'Read006.o', 'Read007.hi', 'Read007.o']),
     multimod_compile,
     ['Read007.hs', '-v0'])
