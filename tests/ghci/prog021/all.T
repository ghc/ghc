test('prog021a',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'prog021a.script'])
     ],
     ghci_script, ['prog021a.script'])
test('prog021b',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'prog021b.script'])
     ],
     ghci_script, ['prog021b.script'])
