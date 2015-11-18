test('prog001',
     [extra_clean(['C.hs', 'D.hs', 'D.hi', 'D.o']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      unless(opsys('mingw32'),extra_ways(['ghci-ext']))],
     ghci_script, ['prog001.script'])
