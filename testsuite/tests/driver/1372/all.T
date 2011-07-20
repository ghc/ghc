test('1372',
     clean_cmd('$MAKE -s clean'),
     run_command,
     ['$MAKE -s --no-print-directory 1372'])

