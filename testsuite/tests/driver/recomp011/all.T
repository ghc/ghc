# Test for #3589, recompiling when #included files change

test('recomp011',
     [ extra_files(['Main.hs'])
     ],
     makefile_test, [])
