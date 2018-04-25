test('recomp004', [extra_files(['Main.hs', 'c.h', 'c1.c', 'c2.c'])], run_command, ['$MAKE -s --no-print-directory recomp004'])
