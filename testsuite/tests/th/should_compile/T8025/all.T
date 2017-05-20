test('T8025', extra_files(['A.hs', 'B.hs']), multimod_compile,
     ['A B', '-fno-code -v0'])