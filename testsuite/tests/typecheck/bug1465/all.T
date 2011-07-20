test('bug1465',
     clean_cmd('$MAKE -s clean'),
     run_command,
     ['$MAKE -s --no-print-directory bug1465'])
