
test('dynamicToo001boot',
     [extra_files(['A.hs', 'B1.hs-boot', 'B2.hs-boot', 'C.hs']),
      when(opsys('mingw32'), expect_broken(7665)), unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     makefile_test, [])
