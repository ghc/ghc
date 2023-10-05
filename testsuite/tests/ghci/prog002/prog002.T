test('prog002', [extra_files(['../shell.hs', 'A1.hs', 'A2.hs', 'B.hs', 'C.hs', 'D.hs']),
                 cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
                 when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     ghci_script, ['prog002.script'])
