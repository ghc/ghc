
test('dynamicToo002',
     [extra_files(['A.hs', 'B.hs', 'C.hs']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)],
     makefile_test, [])
