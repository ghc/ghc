test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']),
      javascript_failures,
      when(opsys('mingw32'), expect_broken(14276))],
     makefile_test, [])
