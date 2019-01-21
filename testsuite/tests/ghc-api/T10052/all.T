test('T10052', [when(arch('powerpc64') or arch('powerpc64le'),
                expect_broken(11259)), req_interp],
     makefile_test, ['T10052'])
