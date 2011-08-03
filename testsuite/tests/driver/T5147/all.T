test('T5147',
     [clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s --no-print-directory T5147'])

