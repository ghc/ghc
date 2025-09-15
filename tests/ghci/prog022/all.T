test('ghci.prog022a',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog022a.script'])
     ],
     makefile_test, ['ghci.prog022a'])
test('ghci.prog022b',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog022b.script'])
     ],
     makefile_test, ['ghci.prog022b'])
