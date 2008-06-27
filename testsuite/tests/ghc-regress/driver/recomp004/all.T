test('recomp004',
     extra_clean(['Main', 'Main.exe', 'c.c']),
     run_command,
     ['$MAKE -s --no-print-directory recomp004'])
