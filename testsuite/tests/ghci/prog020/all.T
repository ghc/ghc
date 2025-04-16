test('ghci.prog020',
     [cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog020.script']),
      when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, ['ghci.prog020'])
