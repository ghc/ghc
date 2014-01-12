test('dynamic_flags_001',
     [when(fast(), skip),
      clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s --no-print-directory dynamic_flags_001'])

