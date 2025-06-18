test('prog-mhu005a',
     [extra_files(['a/', 'b/', 'unitA', 'unitB',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005a'])
