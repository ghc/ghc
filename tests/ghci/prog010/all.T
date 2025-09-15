test('ghci.prog010',
     [cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['../shell.hs', 'A.hs', 'B.hs']),
      when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     ghci_script, ['ghci.prog010.script'])
