# Test for #481, a recompilation bug with Template Haskell

test('recomp009',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp009'])

