# This test is for bug #906, a problem with the ordering of
# compilations involving .hs-boot files, fixed in 6.6.

test('hs-boot', [], multimod_compile, ['Main -v0', ''])
