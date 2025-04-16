test('prog-mhu001',
     [extra_files(['../shell.hs', 'e/', 'unitE', 'prog-mhu001.script']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      req_interp],
     ghci_multiunit_script, [['unitE'], 'prog-mhu001.script'])
