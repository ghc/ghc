
test('dynamicToo003',
     [extra_files(['A003.hs']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)], run_command,
     ['$MAKE -s --no-print-directory dynamicToo003'])
