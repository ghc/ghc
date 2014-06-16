test('rn.prog006',
     extra_clean(['A.hi', 'A.o', 'B/C.hi', 'B/C.o',
                  'Main.hi', 'Main.o', 'pkg.conf',
                  'pwd', 'pwd.exe', 'pwd.exe.manifest',
                  'pwd.hi', 'pwd.o', 'local.package.conf']),
     run_command,
     ['$MAKE -s --no-print-directory rn.prog006'])
