test('apirecomp001',
     when(fast(), skip),
     run_command,
     ['$MAKE -s --no-print-directory apirecomp001'])
