test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']), expect_broken(13168)],
     run_command,
     ['$MAKE -s --no-print-directory T13168'])
