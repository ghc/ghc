# Test for #12723, a recompilation bug

test('recomp016',
     [extra_files(['A.hs', 'A2.hs', 'C.hs', 'D.hs', 'E.hs'])],
     makefile_test, [])
