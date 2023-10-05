# Test for #3589, recompiling when #included files change

test('recomp011',
     [ extra_files(['Main.hs'])
     , js_broken(22261)
     ],
     makefile_test, [])
