# Test for #4469, a recompilation bug related to instances

test('recomp008',
     [extra_files(['A1.hs', 'A2.hs', 'B.hs', 'Main.hs'])
      , when(fast(), skip)
      , normalise_slashes
      , js_broken(22261)],
     makefile_test, [])
