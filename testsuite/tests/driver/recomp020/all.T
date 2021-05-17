# Recompilation test for when .hi files are up to date but .o files are
# not

test('recomp020', [extra_files(['A.hs'])], makefile_test, [])
