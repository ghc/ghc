# Test for #13099, a recompilation bug involving type family instances

test('recomp017',
     [extra_files(['A.hs', 'B.hs', 'C.hs', 'C2.hs', 'D.hs', 'E.hs']),
      ignore_stdout, expect_broken(13099)],
     makefile_test, [])
