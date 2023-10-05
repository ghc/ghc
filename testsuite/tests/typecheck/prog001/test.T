
test('typecheck.prog001', [extra_files(['A.hs', 'B.hs', 'C.hs']),
                           when(fast(), skip)], multimod_compile, ['C', '-v0'])
