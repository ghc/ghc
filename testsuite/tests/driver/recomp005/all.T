test('recomp005',
     [extra_files(['A.hs', 'B.hs', 'C1.hs', 'C2.hs', 'D.hs', 'E.hs']),
      when(fast(), skip)],
     makefile_test, [])
