
proj_files = extra_files(['a/', 'b/', 'unitA', 'unitB'])

test('prog-mhu006a',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu006a'])
