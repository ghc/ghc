# Test for #5614, a recompilation bug

test('recomp010',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp010'])

