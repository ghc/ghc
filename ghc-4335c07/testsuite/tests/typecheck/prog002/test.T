test('typecheck.prog002', [extra_files(['A.hs', 'B.hs']),
                           when(fast(), skip)], multimod_compile, ['B', '-v0'])
