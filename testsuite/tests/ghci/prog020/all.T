test('ghci.prog020',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog020.script'])
     ],
     makefile_test, ['ghci.prog020'])
