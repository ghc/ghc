
test('dynamicToo004',
     [extra_files(['Setup.hs', 'pkg1/', 'pkg1dyn/', 'pkg2/', 'prog.hs']),
      expect_broken(7665),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     makefile_test, [])

