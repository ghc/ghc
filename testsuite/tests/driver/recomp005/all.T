test('recomp005',
     [when(fast(), skip),
      clean_cmd('$MAKE -s clean')],
     run_command, ['$MAKE -s --no-print-directory recomp005'])

