test('T20696', [extra_files(['A.hs', 'B.hs', 'C.hs'])
               , unless(ghc_dynamic(), skip)], multimod_compile, ['A', ''])
test('T20696-static', [extra_files(['A.hs', 'B.hs', 'C.hs'])
               , when(ghc_dynamic(), skip)], multimod_compile, ['A', ''])
