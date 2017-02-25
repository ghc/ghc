# This test is for bug #906, a problem with the ordering of
# compilations involving .hs-boot files, fixed in 6.6.

test('hs-boot', [extra_files(['A.hs', 'A.hs-boot', 'B.hs', 'C.hs', 'Main.hs'])], multimod_compile, ['Main -v0', ''])
