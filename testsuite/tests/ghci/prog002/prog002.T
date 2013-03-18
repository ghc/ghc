test('prog002',
     [extra_clean(['A.hs', 'A.hi', 'A.o']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script, ['prog002.script'])

