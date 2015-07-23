
test('dynamicToo005',
     [extra_clean(['A005.o', 'A005.hi', 'A005.dyn_o', 'A005.dyn_hi']),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo005'])

