test('T10052', when(arch('powerpc64'), expect_broken(11259)), run_command,
               ['$MAKE -s --no-print-directory T10052'])
