# Test for #481, a recompilation bug with Template Haskell

test('recomp009', [], run_command, ['$MAKE -s --no-print-directory recomp009'])
