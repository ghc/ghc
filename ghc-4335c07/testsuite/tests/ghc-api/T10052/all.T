test('T10052', [when(arch('powerpc64') or arch('powerpc64le'),
                expect_broken(11259)), req_interp],
     run_command, ['$MAKE -s --no-print-directory T10052'])
