test('prog002', [extra_files(['../shell.hs', 'A1.hs', 'A2.hs', 'B.hs', 'C.hs', 'D.hs']),
                 cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script, ['prog002.script'])
