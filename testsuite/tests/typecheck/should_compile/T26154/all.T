test('T26154_make', [extra_files(['A.hs', 'B.hs', 'B.hs-boot', 'Other.hs', 'T26154.hs'])], makefile_test, ['T26154'])
test('T26154', [extra_files(['A.hs', 'B.hs', 'B.hs-boot', 'Other.hs', 'T26154.hs'])], multimod_compile, ['T26154', '-v0'])
