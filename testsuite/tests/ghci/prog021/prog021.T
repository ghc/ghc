test('prog021',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['A.hs', 'B.hs', 'prog021.script'])
     ],
     ghci_script, ['prog021.script'])
