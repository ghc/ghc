# Recompilation test for when .hi files are up to date but .o files are
# not

test('recomp019',
     [extra_files(['Main.hs', 'B1.hs', 'B2.hs', 'C.hs'])],
     makefile_test, [])
