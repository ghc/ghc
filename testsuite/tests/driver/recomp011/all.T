# Test for #3589, recompiling when #included files change

test('recomp011',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp011'])

