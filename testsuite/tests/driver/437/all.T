# Test for #437, a recompilation bug with '-main-is'

test('437',
     [ clean_cmd('$MAKE -s clean') ],
     run_command,
     ['$MAKE -s --no-print-directory 437'])

