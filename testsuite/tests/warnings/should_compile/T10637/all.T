test('T10637', [extra_files(['A.hs', 'A.hs-boot'])], multimod_compile, ['T10637', '-v0 -fwarn-unused-imports'])
