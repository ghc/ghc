test('T12062', [extra_files(['A.hs', 'A.hs-boot', 'C.hs'])], multimod_compile_fail, ['T12062', '-v0 -j0'])
