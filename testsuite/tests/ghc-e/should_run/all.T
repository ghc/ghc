
setTestOpts(when(compiler_profiled(), skip))

test('ghc-e001', req_interp, run_command, ['$MAKE --no-print-directory -s ghc-e001'])
test('ghc-e002', req_interp, run_command, ['$MAKE --no-print-directory -s ghc-e002'])
test('ghc-e003', req_interp, run_command, ['$MAKE --no-print-directory -s ghc-e003'])
test('ghc-e004', req_interp, run_command, ['$MAKE --no-print-directory -s ghc-e004'])
test('ghc-e005', req_interp, run_command, ['$MAKE --no-print-directory -s ghc-e005'])

test('T2228',
     [req_interp, when(ghci_dynamic(), expect_broken(7298))],
     run_command,
     ['$MAKE --no-print-directory -s T2228'])
test('T2636', req_interp, run_command, ['$MAKE --no-print-directory -s T2636'])
test('T3890', req_interp, run_command, ['$MAKE --no-print-directory -s T3890'])
test('T7299', req_interp, run_command, ['$MAKE --no-print-directory -s T7299'])
