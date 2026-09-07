test('prog-mhu008',
     [extra_files(['a/', 'b/', 'unitA', 'unitB']),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB'], 'prog-mhu008.script'])
