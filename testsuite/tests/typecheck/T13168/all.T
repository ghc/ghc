test('T13168',
     extra_files(['package1', 'package2', 'Setup.hs']),
     run_command,
     ['$MAKE -s --no-print-directory T13168'])
