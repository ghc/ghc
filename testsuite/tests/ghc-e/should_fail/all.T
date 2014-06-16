setTestOpts(when(compiler_profiled(), skip))

test('T7962', [exit_code(2), req_interp, ignore_output], run_command, ['$MAKE --no-print-directory -s T7962'])
