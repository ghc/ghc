test('prog-mhu004a',
     [extra_files(['a/', 'b/', 'unitA', 'unitB',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB'], 'prog-mhu004a.script'])
test('prog-mhu004b',
     [extra_files(['a/', 'b/', 'unitA', 'unitB',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB'], 'prog-mhu004b.script'])
