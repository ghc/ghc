# Test for #4469, a recompilation bug related to instances

test('recomp008',
     [extra_files(['A1.hs', 'A2.hs', 'B.hs', 'Main.hs']),
      when(fast(), skip), normalise_slashes],
     makefile_test, [])
