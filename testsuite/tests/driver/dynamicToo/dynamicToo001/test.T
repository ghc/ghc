
test('dynamicToo001',
     [when(opsys('mingw32'), expect_broken(7665)), unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command, ['$MAKE -s --no-print-directory dynamicToo001'])
