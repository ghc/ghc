test('T7859', req_interp, run_command,
     ['$MAKE --no-print-directory -s T7859'])

test('T8601', req_interp, run_command,
     ['$MAKE --no-print-directory -s T8601'])

test('T-signals-child',
     [ when(opsys('mingw32'), skip), req_interp
     , only_ways(['threaded1', 'threaded2'])
     ],
     run_command,
     ['$MAKE --no-print-directory -s T-signals-child'])
