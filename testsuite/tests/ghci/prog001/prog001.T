test('prog001',
     [extra_files(['../shell.hs', 'A.hs', 'B.hs', 'C1.hs', 'D1.hs', 'D2.hs']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      unless(opsys('mingw32') or (not config.have_ext_interp), extra_ways(['ghci-ext']))],
     ghci_script, ['prog001.script'])
