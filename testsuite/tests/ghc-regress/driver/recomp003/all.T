test('recomp003',
     extra_clean(['Char.hs', 'err']),
     run_command,
     ['$MAKE -s --no-print-directory recomp003'])
