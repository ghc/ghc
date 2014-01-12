# Test for #7215, a recompilation bug

test('recomp012',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory recomp012'])

