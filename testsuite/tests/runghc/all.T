test('T7859', req_interp, makefile_test, [])

test('T8601', req_interp, makefile_test, [])

test('T11247', [req_interp, expect_broken(11247)], makefile_test, [])

test('T6132', [when(opsys('darwin'), expect_broken(6132))], compile, [''])

test('T-signals-child',
     [ when(opsys('mingw32'), skip), req_interp
     , only_ways(['threaded1', 'threaded2'])
     ],
     makefile_test, [])
