setTestOpts(when(compiler_profiled(), skip))

test('T7962', [exit_code(2), req_interp, ignore_output], run_command,
     ['$MAKE --no-print-directory -s T7962'])

test('T9905fail1', [exit_code(2), req_interp, ignore_output], run_command,
     ['$MAKE --no-print-directory -s T9905fail1'])

test('T9905fail2', [exit_code(2), req_interp, ignore_output], run_command,
     ['$MAKE --no-print-directory -s T9905fail2'])

test('T9905fail3', [exit_code(2), req_interp, ignore_output], run_command,
     ['$MAKE --no-print-directory -s T9905fail3'])

test('ghc-e-fail1', [exit_code(2), req_interp, ignore_output], run_command,
     ['$MAKE --no-print-directory -s ghc-e-fail1'])

test('ghc-e-fail2', [exit_code(2), req_interp, ignore_output], run_command,
     ['$MAKE --no-print-directory -s ghc-e-fail2'])
