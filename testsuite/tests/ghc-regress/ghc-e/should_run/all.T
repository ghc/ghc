
setTestOpts(if_compiler_profiled(skip))

test('ghc-e001', normal, run_command, ['$MAKE --no-print-directory -s ghc-e001'])
test('ghc-e002', normal, run_command, ['$MAKE --no-print-directory -s ghc-e002'])
test('ghc-e003', normal, run_command, ['$MAKE --no-print-directory -s ghc-e003'])
test('ghc-e004', normal, run_command, ['$MAKE --no-print-directory -s ghc-e004'])
test('ghc-e005', normal, run_command, ['$MAKE --no-print-directory -s ghc-e005'])

test('2228', normal, run_command, ['$MAKE --no-print-directory -s 2228'])
test('2636', normal, run_command, ['$MAKE --no-print-directory -s 2636'])
test('3890', normal, run_command, ['$MAKE --no-print-directory -s 3890'])
