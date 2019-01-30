
test('dynamicToo005',
     [extra_files(['dynamicToo005.bkp']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)],
     makefile_test, [])
