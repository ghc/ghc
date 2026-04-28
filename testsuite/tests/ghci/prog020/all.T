test('ghci.prog020a',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog020a.script'])
     ],
     makefile_test, ['ghci.prog020a'])

test('ghci.prog020b',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog020b.script'])
     ],
     makefile_test, ['ghci.prog020b'])
