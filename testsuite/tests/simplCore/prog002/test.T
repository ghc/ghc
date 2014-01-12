test('simplCore.prog002',
     extra_clean(['Simpl009Help.hi', 'Simpl009Help.o',
                  'simpl009.hi', 'simpl009.o']),
     multimod_compile,
     ['simpl009', '-v0'])
