test('1959',
     extra_clean(['E.hs', 'prog']),
     run_command,
     ['$MAKE -s --no-print-directory dotest'])
