test('T7478', [unless(have_dynamic(), skip)], run_command,
     ['$MAKE -s --no-print-directory T7478'])
