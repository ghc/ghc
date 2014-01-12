test('T7373',
     [extra_clean(['pkg/Setup', 'pkg/Setup.exe', 'pkg/Setup.o', 'pkg/Setup.hi',
                   'D.o', 'D.hi', 'package.conf']),
      clean_cmd('rm -rf pkg/dist/'),
      expect_broken(7373)],
     run_command,
     ['$MAKE -s --no-print-directory T7373'])

