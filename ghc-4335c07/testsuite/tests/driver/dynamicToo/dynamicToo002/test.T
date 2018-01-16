
test('dynamicToo002',
     [extra_files(['A.hs', 'B.hs', 'C.hs']),
      unless(have_vanilla(), skip), unless(have_dynamic(), skip)], run_command,
     ['$MAKE -s --no-print-directory dynamicToo002'])
