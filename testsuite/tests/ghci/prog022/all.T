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
test('ghci.prog022c',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog022c.script'])
     ],
     makefile_test, ['ghci.prog022c'])
test('ghci.prog022d',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog022d.script'])
     ],
     makefile_test, ['ghci.prog022d'])
test('ghci.prog022e',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog022e.script'])
     ],
     makefile_test, ['ghci.prog022e'])
test('ghci.prog022f',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'ghci.prog022f.script'])
     ],
     makefile_test, ['ghci.prog022f'])
