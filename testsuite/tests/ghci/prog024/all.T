test('prog024a',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog024a.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog024a.script'])

test('prog024b',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog024b.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog024b.script'])

test('prog024c',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog024c.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog024c.script'])

test('prog024d',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog024d.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog024d.script'])

test('prog024e',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog024e.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog024e.script'])

test('prog024f',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog024f.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog024f.script'])
