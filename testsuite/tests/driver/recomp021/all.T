# Recompilation test for when .hi files are up to date but .o files are
# not

test('recomp021', [extra_files(['A.hs', 'B.hs'])], makefile_test, [])
