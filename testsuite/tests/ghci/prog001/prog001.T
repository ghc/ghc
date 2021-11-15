test('prog001',
     [extra_files(['../shell.hs', 'A.hs', 'B.hs', 'C1.hs', 'D1.hs', 'D2.hs']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      req_rts_linker,
      skip, # ghcjs
      unless(opsys('mingw32'), extra_ways(['ghci-ext']))],
     ghci_script, ['prog001.script'])
