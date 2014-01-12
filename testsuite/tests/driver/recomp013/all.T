# Test for #8247, a recompilation bug

test('recomp013',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp013'])

