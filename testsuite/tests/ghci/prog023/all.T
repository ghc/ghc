test('prog023a',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog023a.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog023a.script'])

test('prog023b',
     [req_interp,
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      extra_files(['src/', 'prog023a.script']),
      extra_hc_opts('-isrc')
     ],
     ghci_script, ['prog023b.script'])
