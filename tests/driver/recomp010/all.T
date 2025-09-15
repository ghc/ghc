# Test for #5614, a recompilation bug

test('recomp010', [extra_files(['Main.hs', 'X1.hs', 'X2.hs'])], makefile_test, [])
