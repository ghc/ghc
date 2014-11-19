# Test for #8144, a recompilation bug

test('recomp015',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp015'])

