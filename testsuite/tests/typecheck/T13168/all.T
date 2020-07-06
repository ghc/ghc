test('T13168',
     [extra_files(['package1', 'package2', 'Setup.hs']),
      when(cross(), skip), # <command line>: not built for interactive use
      when(opsys('mingw32'), expect_broken(14276))],
     makefile_test, [])
