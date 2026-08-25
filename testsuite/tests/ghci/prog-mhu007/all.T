
proj_files = extra_files(['a/', 'b/', 'unitA', 'unitB', 'testpkg-bar/', 'testpkg-foo/'])

test('prog-mhu007',
     [proj_files,
     cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
     req_interp],
     makefile_test, ['prog-mhu007'])
