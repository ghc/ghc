test('T8308', when(opsys('mingw32'), expect_broken(8308)),
     run_command, ['$MAKE -s --no-print-directory T8308'])
