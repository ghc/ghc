test('retc001',
     [clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s --no-print-directory retc001'])

