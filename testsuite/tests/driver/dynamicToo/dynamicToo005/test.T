
test('dynamicToo005',
     [unless(have_vanilla(), skip), unless(have_dynamic(), skip)], run_command,
     ['$MAKE -s --no-print-directory dynamicToo005'])
