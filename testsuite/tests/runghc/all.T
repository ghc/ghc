
setTestOpts(when(compiler_profiled(), skip))

test('T7859', req_interp, run_command,
     ['$MAKE --no-print-directory -s T7859'])

test('T8601', req_interp, run_command,
     ['$MAKE --no-print-directory -s T8601'])
