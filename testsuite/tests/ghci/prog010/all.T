test('ghci.prog010',
     [cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs'])],
     ghci_script, ['ghci.prog010.script'])
