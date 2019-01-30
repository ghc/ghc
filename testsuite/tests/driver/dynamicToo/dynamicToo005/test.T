
test('dynamicToo005',
     [extra_files(['dynamicToo005.bkp']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)], run_command,
     ['$MAKE -s --no-print-directory dynamicToo005'])
