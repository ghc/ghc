test('dynamic_flags_001',
     [skip_if_fast,
      clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s --no-print-directory dynamic_flags_001'])

