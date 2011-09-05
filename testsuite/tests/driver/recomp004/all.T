test('recomp004',
     extra_clean(['MainX', 'MainX.exe', 'MainX.hi', 'MainX.o', 'MainX.hs',
                  'c.o', 'c.c']),
     run_command,
     ['$MAKE -s --no-print-directory recomp004'])
