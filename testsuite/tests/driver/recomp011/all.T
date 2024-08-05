# Test for #3589, recompiling when #included files change

test('recomp011',
     [ extra_files(['Main.hs'])
     , js_broken(23013)
     ],
     makefile_test, [])
