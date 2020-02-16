test('T17586',
     [when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11261))],
     makefile_test, [])
