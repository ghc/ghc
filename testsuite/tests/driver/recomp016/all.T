# Test for #12723, a recompilation bug

test('recomp016',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp016'])

