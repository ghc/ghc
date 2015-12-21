test('T7859', req_interp, run_command,
     ['$MAKE --no-print-directory -s T7859'])

test('T8601', req_interp, run_command,
     ['$MAKE --no-print-directory -s T8601'])

test('T11247', [req_interp, expect_broken(11247)], run_command,
     ['$MAKE --no-print-directory -s T11247'])
