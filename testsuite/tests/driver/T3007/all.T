test('T3007',
     [extra_files(['A/', 'B/']),
      expect_broken(14759)],
     run_command,
     ['$MAKE -s --no-print-directory T3007'])
