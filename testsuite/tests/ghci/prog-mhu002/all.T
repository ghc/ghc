test('prog-mhu002a',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002a.script'])
test('prog-mhu002b',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002b.script'])
test('prog-mhu002c',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     # See Note [Disabling ghci-leak-check]
     extra_hc_opts('-fno-ghci-leak-check -XGHC2021'),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002c.script'])
test('prog-mhu002d',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     normalise_slashes,
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002d.script'])
test('prog-mhu002e',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     # GHCi target guessing seems to be tripping over the slashes.
     # Separate bug that needs to be fixed.
     when(opsys('mingw32'), skip),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002e.script'])
test('prog-mhu002f',
     [extra_files(['a/', 'b/', 'c/', 'd/'
                  , 'unitA', 'unitB', 'unitC', 'unitD',
                  ]),
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC', 'unitD'], 'prog-mhu002f.script'])
