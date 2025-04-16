test('prog-mhu002',
     [extra_files(['../shell.hs', 'a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     unless(opsys('mingw32') or not config.have_RTS_linker, extra_ways(['ghci-ext'])),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002.script'])
