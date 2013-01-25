test('T1959',
     extra_clean(['E.hi', 'E.o', 'E.hs', 'prog', 'compile.out',
                  'B.hi', 'B.o', 'C.hi', 'C.o', 'D.hi', 'D.o']),
     run_command,
     ['$MAKE -s --no-print-directory dotest'])
