
test('dynamicToo001',
     [extra_files(['A.hs', 'B.hs', 'B1.hs', 'B2.hs', 'C.hs']),
      when(opsys('mingw32'), expect_broken(7665)), unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     makefile_test, [])
