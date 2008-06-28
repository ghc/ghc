test('rn.prog006',
     extra_clean(['A.hi', 'A.o', 'B/C.hi', 'B/C.o',
                  'Main.hi', 'Main.o', 'pkg.conf']),
     run_command,
     ['$MAKE -s --no-print-directory rn.prog006'])
