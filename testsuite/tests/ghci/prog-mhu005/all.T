
proj_files = extra_files(['a/', 'b/', 'unitA', 'unitB'])

test('prog-mhu005a',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005a'])

test('prog-mhu005b',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005b'])

test('prog-mhu005c',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005c'])

test('prog-mhu005d',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005d'])

test('prog-mhu005e',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005e'])

test('prog-mhu005f',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005f'])

test('prog-mhu005g',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu005g'])
