
proj_files = extra_files(['a/', 'unitA', 'testpkg/'])

test('prog025a',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog025a'])

test('prog025b',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog025b'])
