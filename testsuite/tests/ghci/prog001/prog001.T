test('prog001',
     [extra_clean(['C.hs', 'D.hs', 'D.hi', 'D.o']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script, ['prog001.script'])

