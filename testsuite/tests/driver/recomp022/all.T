# Recompilation test for when .hi files are up to date but .o files are
# not

test('recomp022a', [extra_files(['A1.hs', 'A2.hs', 'B.hs']), normalise_slashes], makefile_test, [])
test('recomp022b', [extra_files(['A1.hs', 'A3.hs', 'B.hs', 'C.hs']), normalise_slashes], makefile_test, [])
