test('simplCore.prog001',
     extra_clean(['Simpl006Help.hi', 'Simpl006Help.o',
                  'simpl006.hi', 'simpl006.o']),
     multimod_compile,
     ['simpl006', '-v0'])
