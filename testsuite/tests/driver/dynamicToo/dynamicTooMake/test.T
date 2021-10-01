
test('dynamicTooMake',
     [extra_files(['p', 'q', 'Setup.hs']),
      when(opsys('mingw32'), expect_broken(7665)), unless(have_vanilla(), skip),
      unless(have_dynamic(), skip),
      copy_files,
      grep_errmsg("Dynamic hash")
      ],
     makefile_test, [])
