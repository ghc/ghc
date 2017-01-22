test('T13082_good',
     [extra_files(['foo.c', 'main.hs', 'foo_dll.c']),
      unless(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory T13082_good'])

test('T13082_fail', [extra_files(['main.hs']), unless(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory T13082_fail'])
