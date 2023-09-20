test('T14304',
     [extra_files(['p', 'indef', 'th', 'Setup.hs']),
      unless(have_dynamic(), skip)],
     makefile_test,
     [])
