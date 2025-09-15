
test('dynamicToo003',
     [extra_files(['A003.hs']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)],
     makefile_test, [])

test('dynamicTooRecomp',
     [extra_files(['ARecomp.hs']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)],
     makefile_test, [])

test('dynamicTooOnlyInterface',
     [extra_files(['ARecomp.hs']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)],
     makefile_test, [])
