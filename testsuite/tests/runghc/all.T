test('T7859', req_interp, makefile_test, [])

test('T8601', req_interp, makefile_test, [])

test('T11247', [req_interp, expect_broken(11247)], makefile_test, [])

test('T6132', [],
     compile, [''])

test('T17171a',
     [req_interp, exit_code(2), ignore_stdout, grep_errmsg(r'main')],
     makefile_test, [])

test('T17171b', req_interp, makefile_test, [])

test('T-signals-child',
     [ when(opsys('mingw32'), skip), req_interp
     , only_ways(['threaded1', 'threaded2'])
     , fragile_for(17307, ['threaded1', 'threaded2', 'nonmoving_thr'])
     ],
     makefile_test, [])
