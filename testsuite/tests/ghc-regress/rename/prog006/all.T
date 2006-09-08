test('rn.prog006', normal, run_command, ['$MAKE -s --no-print-directory rn.prog006'])
clean(['A.hi', 'A.o', 'Main.hi', 'Main.o', 'pkg.conf', 'B/C.hi', 'B/C.o'])
