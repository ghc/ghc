test('prog-mhu003',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu003.script'])
