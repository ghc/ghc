test('T10672_x64',
     [extra_files(['Main.hs', 'Printf.hs', 'cxxy.cpp']),
      [unless(opsys('mingw32'), skip), unless(arch('x86_64'), skip)]],
     run_command, ['$MAKE -s --no-print-directory T10672_x64'])

test('T10672_x86',
     [extra_files(['Main.hs', 'Printf.hs', 'cxxy.cpp']),
      [unless(opsys('mingw32'), skip), unless(arch('i386'), skip)]],
     run_command, ['$MAKE -s --no-print-directory T10672_x86'])
