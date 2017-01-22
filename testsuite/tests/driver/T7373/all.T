test('T7373', [expect_broken(7373)], run_command,
     ['$MAKE -s --no-print-directory T7373'])
