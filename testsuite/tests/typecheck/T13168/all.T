test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']),
      when(opsys('mingw32'), expect_broken(14276))],
     run_command,
     ['$MAKE -s --no-print-directory T13168'])
