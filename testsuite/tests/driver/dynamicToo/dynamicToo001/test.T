opts = [ extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs']),
      when(opsys('mingw32'), expect_broken(7665)), unless(have_vanilla(), skip),
      unless(have_dynamic(), skip) ]

test('dynamicToo001', [opts], makefile_test, [])
test('dynamicToo001MakeA', [opts], makefile_test, [])
test('dynamicToo001MakeB', [opts], makefile_test, [])
test('T25837', [opts], makefile_test, [])
