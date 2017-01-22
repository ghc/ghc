test('dynamic_flags_001', [when(fast(), skip)], run_command,
     ['$MAKE -s --no-print-directory dynamic_flags_001'])
